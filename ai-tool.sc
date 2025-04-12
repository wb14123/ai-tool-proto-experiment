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
    httpPostPath: String,
    // httpPostHeaders: Map[String, String],
    HttpPostBody: String,
)

case class ChatResponse(
    callTool: Option[ToolParam] = None,
    toUser: Option[String] = None,
)

object ChatResponseSchema {
  def apply(): Json= {
    json.Json.schema[ChatResponse].asCirce(Draft04())
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
    // text: TextParam = TextParam(),
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
    |follow the Json schema definition, which either request a call to one of the APIs, or response to user directly
    |if there is no need to request to any tool. Response the Json content only without any code block quote.
    |
    |Here is the Json schema you must follow for the response:
    |
    |$jsonSchema
    |
    |Here are the OpenAPI definition of the tools:
    |
    |--- End of tool definition.
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
  val req = ChatRequest(
    input = Seq(
      ChatMessage(role = "user", content = "hello"),
    ),
    instructions = getSystemPrompt(Seq()),
  )
  val res = sendLLMRequest(req)
  println(res)
}


run()