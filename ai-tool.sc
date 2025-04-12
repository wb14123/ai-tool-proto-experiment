#!/usr/bin/env amm

import $ivy.`com.lihaoyi::requests:0.9.0`
import $ivy.`io.circe::circe-core:0.14.12`
import $ivy.`io.circe::circe-generic:0.14.12`
import $ivy.`io.circe::circe-parser:0.14.12`
import $ivy.`com.github.andyglow::scala-jsonschema:0.7.11`
import $ivy.`com.github.andyglow::scala-jsonschema-circe-json:0.7.11`

import com.github.andyglow.jsonschema.AsCirce._
import json.schema.Version._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe._
import io.circe.parser._

import scala.util.Try


case class ChatMessage (
    role: String,
    content: String,
)

case class ToolParam(
    httpHostname: String,
    httpPostPath: String,
    httpPostHeaders: Map[String, String],
    HttpPostBody: String,
)

case class ChatResponse(
    callTool: Option[ToolParam] = None,
    toUser: Option[String] = None,
)

object ChatResponseSchema {
  def apply(): Json= {
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
      |        "httpHostname": {
      |          "type": ["string", "null"]
      |        },
      |        "httpPostPath": {
      |          "type": ["string", "null"]
      |        },
      |        "httpPostHeaders": {
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
      |        "httpHostname",
      |        "httpPostPath",
      |        "httpPostHeaders",
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
}

case class TextFormat(
    `type`: String = "json_schema",
    name: String = "entities",
    schema: Json = ChatResponseSchema(),
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
  val jsonSchema = ChatResponseSchema().toString

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
      ChatMessage(role = "user", content = "What is the weather today?"),
    ),
    instructions = getSystemPrompt(Seq()),
  )
  val res = sendLLMRequest(req)
  println(res)
}


run()