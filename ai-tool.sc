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

import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import scala.util.{Failure, Try}
import scala.io.StdIn.readLine
import scala.annotation.tailrec


case class ChatMessage (
    role: String,
    content: String,
)

case class ToolParam(
    httpRequestHost: String,
    httpRequestPath: String,
    httpRequestHeaders: Option[Map[String, String]],
    httpRequestMethod: String,
    HttpPostBody: Option[String],
)

case class ChatResponse(
    callTool: Option[ToolParam] = None,
    toUser: Option[String] = None,
)

val chatResponseSchemaStr: String =
  // json.Json.schema[ChatResponse].asCirce(Draft04())
  """
    |{
    |  "type": "object",
    |  "properties": {
    |    "callTool": {
    |      "type": ["object", "null"],
    |      "properties": {
    |        "httpRequestHost": {
    |          "type": ["string"]
    |        },
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
    |        "httpRequestHost",
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
    |""".stripMargin

val chatResponseSchema: Json = parse(chatResponseSchemaStr).toTry.get

case class TextFormat(
    `type`: String = "json_schema",
    name: String = "entities",
    schema: Json = chatResponseSchema,
    strict: Boolean = true,
)

case class TextParam(
    format: TextFormat = TextFormat()
)


val systemPrompt: String = {
  /*
  TODO: authentication flow.

  It's better to add another action other than `callTool` and `toUser`, which opens the authentication url directly
  in the browser and use a callback url to send the instruction back to the agent without user copying anything.

  But since this is just a PoC, just keep it simple here.
   */
  s"""You are a helpful assistant.
     |
     |You have many tools to use by sending a http request to some API servers. Your response must be Json that
     |follows the Json schema definition:
     |
     |$chatResponseSchemaStr
     |
     |Either request a call to one of the APIs with `callTool` field, or
     |response to user directly with `toUser` field if there is no need to request to any tool or you need more
     |information from the user.
     |
     |Each tool has an optional login URL that you can ask the user to open in the browser. Then user will copy the
     |authentication instruction back to you and you can follow the instruction to do the authentication with the
     |tool server.
     |
     |Important:
     |
     |* Response only the JSON body. Never quote the response in something like json```...```.
     |* Never response to user directly without using the `toUser` field with a Json response.
     |* Only one of `callTool` and `toUser` field should be filled.
     |* Always include the `http` or `https` part for the `httpRequestHost` field.
     |
     |""".stripMargin
}

case class ChatRequest(
    input: Seq[ChatMessage],
    instructions: String = systemPrompt,
    model: String = "gpt-4o",
    text: TextParam = TextParam(),
)

case class ToolDef(
    httpHost: String,
    openAPIPath: String,
    authUrl: Option[String] = None,
) {
  def prompt: String = {
    val authUrlPrompt = authUrl.map(url => s"Tool login URL: $url\n").getOrElse("")
    s"""----
       |Tool server host: $httpHost
       |
       |$authUrlPrompt
       |Tool's OpenAPI definition:
       |$openAPIDef
       |
       |----
       |
       |""".stripMargin
  }

  private def openAPIDef: String = {
    requests.get(httpHost + openAPIPath).text()
  }
}

def sendLLMRequest(req: ChatRequest): Try[ChatResponse] = {
  val llmEndpoint = "https://api.openai.com/v1/responses"
  val token = sys.env("OPENAI_API_KEY")
  val timeStr = ZonedDateTime.now().format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
  val reqWithTime = req.copy(instructions = req.instructions + s"\n\nThe current time is $timeStr .")
  val postData =  reqWithTime.asJson.toString
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
  val hostname = toolParam.httpRequestHost
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
  val tools = Seq(
    ToolDef(httpHost = "https://grpc.rssbrain.com", openAPIPath = "/swagger.json",
      authUrl = Some("http://app.rssbrain.com/login?redirect_url=/llm_auth")),
  )
  val toolsPrompt = tools.map(_.prompt).mkString("\n")

  val req = ChatRequest(
    input = Seq(
      ChatMessage(role = "developer", content =
        s"""
          |
          |Here are the OpenAPI definition of the tools:
          |
          |$toolsPrompt
          |
          |""".stripMargin),
    ),
    instructions = systemPrompt,
  )
  loop(req, None, waitForUser = true)
}


run()