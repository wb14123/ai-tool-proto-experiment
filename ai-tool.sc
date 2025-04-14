#!/usr/bin/env amm

import $ivy.`com.lihaoyi::requests:0.9.0`
import $ivy.`io.circe::circe-core:0.14.12`
import $ivy.`io.circe::circe-generic:0.14.12`
import $ivy.`io.circe::circe-parser:0.14.12`
import $ivy.`com.github.andyglow::scala-jsonschema:0.7.11`
import $ivy.`com.github.andyglow::scala-jsonschema-circe-json:0.7.11`
import com.github.andyglow.jsonschema.AsCirce._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe._
import io.circe.parser._

import scala.util.{Success, Try, Failure}
import scala.io.StdIn.readLine
import scala.annotation.tailrec


case class ChatMessage (
    role: String,
    content: String,
)

case class ToolParam(
    httpRequestPath: String,
    httpRequestHeaders: Option[Map[String, String]],
    httpRequestMethod: String,
    HttpPostBody: Option[String],
)

case class ChatResponse(
    callTool: Option[ToolParam] = None,
    toUser: Option[String] = None,
)

object ChatResponseSchema {
  val json: Json =
    // json.Json.schema[ChatResponse].asCirce(Draft04())
    parse("""
      |{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema ",
      |  "$id": "https://example.com/ChatResponse.schema.json ",
      |  "title": "ChatResponse",
      |  "type": "object",
      |  "properties": {
      |    "callTool": {
      |      "type": ["object", "null"],
      |      "properties": {
      |        "httpRequestPath": {
      |          "type": ["string"]
      |        },
      |        "httpRequestMethod": {
      |          "type": ["string"]
      |        },
      |        "httpRequestHeaders": {
      |          "type": ["object", "null"],
      |          "additionalProperties": {
      |            "type": ["string", "null"]
      |          }
      |        },
      |        "HttpPostBody": {
      |          "type": ["string", "null"]
      |        }
      |      },
      |      "required": [
      |        "httpRequestPath",
      |        "httpRequestMethod",
      |        "httpRequestHeaders",
      |        "HttpPostBody"
      |      ],
      |      "additionalProperties": false
      |    },
      |    "toUser": {
      |      "type": ["string", "null"]
      |    }
      |  },
      |  "required": [
      |    "callTool",
      |    "toUser"
      |  ],
      |  "additionalProperties": false
      |}
      |""".stripMargin).toTry.get
}

case class TextFormat(
    `type`: String = "json_schema",
    name: String = "entities",
    schema: Json = ChatResponseSchema.json,
    strict: Boolean = true,
)

case class TextParam(
    format: TextFormat = TextFormat()
)

case class ChatRequest(
    input: Seq[ChatMessage],
    instructions: String = "You are a helpful assistant.",
    model: String = "gpt-4o",
    text: TextParam = TextParam(),
)

case class ToolDef(
    openAPIFile: String,
    httpHost: String,
)

def getSystemPrompt(tools: Seq[ToolDef]): String = {
  s"""You are a helpful assistant.
    |
    |You have many tools to use by sending a http request to some API servers. You must response as a Json format that
    |follow the Json schema definition, which either request a call to one of the APIs with `callTool` field, or
    |response to user directly with `toUser` field if there is no need to request to any tool or you need more information from the user.
    |
    |""".stripMargin
}

def sendLLMRequest(req: ChatRequest): Try[ChatResponse] = {
  val llmEndpoint = "https://api.openai.com/v1/responses"
  val token = sys.env("OPENAI_API_KEY")
  val postData =  req.asJson.toString
  println("Sending request ...")
  println(postData)
  val r = requests.post(
    llmEndpoint,
    headers = Map(
      "Authorization" -> s"Bearer $token",
      "Content-Type" -> "application/json",
    ),
    data = postData,
    readTimeout = 600000,
  )
  val resText = r.text()
  println("Response:")
  println(resText)
  parse(resText).toTry.flatMap { j =>
    j.hcursor
      .downField("output")
      .downArray
      .downField("content")
      .downArray
      .downField("text")
      .as[String]
      .toTry
      .flatMap(decode[ChatResponse](_).toTry)
  }
}

def callTool(toolParam: ToolParam): Try[String] = {
  val hostname = "http://localhost:8000"
  if (toolParam.httpRequestMethod.toLowerCase.equals("get")) {
    Try(requests.get(hostname + toolParam.httpRequestPath, headers=toolParam.httpRequestHeaders.getOrElse(Map())).text())
  } else if (toolParam.httpRequestMethod.toLowerCase.equals("post")) {
    val postBody: String = toolParam.HttpPostBody.getOrElse("")
    Try(requests.post(
      hostname + toolParam.httpRequestPath,
      headers = toolParam.httpRequestHeaders.getOrElse(Map()),
      data = postBody,
    ).text())
  } else {
    Failure(new Exception("httpRequestMethod must be one of GET or POST"))
  }
}

@tailrec
def loop(req: ChatRequest, lastResponse: Option[Try[ChatResponse]], waitForUser: Boolean): Unit = {
  if (waitForUser) {
    println("Wait for user input:")
    val userInput = readLine()
    val nextReq = req.copy(input = req.input :+ ChatMessage(role = "user", content = userInput))
    val res = sendLLMRequest(nextReq)
    loop(nextReq, Some(res), waitForUser = false)
  } else {
    if (lastResponse.isEmpty || lastResponse.get.isFailure) {
      println("Error: unexpected response from LLM. Exit the conversation.")
      println(lastResponse)
    } else {
      val res = lastResponse.get.get
      val resInput = ChatMessage(role = "assistant", content = res.asJson.toString)
      println(res)
      if (res.toUser.isDefined) {
        val nextReq = req.copy(input = req.input :+ resInput)
        loop(nextReq, None, waitForUser = true)
      } else if (res.callTool.isDefined) {
        val toolOutput = callTool(res.callTool.get).toString
        val nextReq = req.copy(input = req.input :+ resInput :+ ChatMessage(role = "developer", s"Tool Result:\n$toolOutput"))
        val nextRes = sendLLMRequest(nextReq)
        println(toolOutput)
        loop(nextReq, Some(nextRes), waitForUser = false)
      }
    }
  }
}

def run() = {
  val openApiFile = scala.io.Source.fromFile("swagger.json")
  val openApiDefs = try openApiFile.mkString finally openApiFile.close()
  val req = ChatRequest(
    input = Seq(
      ChatMessage(role = "developer", content =
        s"""
          |
          |Here are the OpenAPI definition of the tools:
          |
          |$openApiDefs
          |
          |""".stripMargin),
    ),
    instructions = getSystemPrompt(Seq()),
  )
  loop(req, None, waitForUser = true)
}


run()