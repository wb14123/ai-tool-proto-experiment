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

import java.io.FileWriter
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import scala.util.{Failure, Try}
import scala.io.StdIn.readLine
import scala.annotation.tailrec

val LLM_ENDPOINT = "https://api.openai.com"
val LLM_MODEL = "gpt-4o"

/*
val LLM_ENDPOINT = "http://127.0.0.1:11434"
val LLM_MODEL = "deepseek-r1:7b"
 */

val logFile = new FileWriter("./log.txt", true)
def logToFile(msg: String): Unit = {
  logFile.write(msg + "\n")
  logFile.flush()
}

case class ChatMessage (
    role: String,
    content: String,
)

case class ToolParam(
    httpRequestEndpoint: String,
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
    |        "httpRequestEndpoint": {
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
    |        "httpRequestEndpoint",
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

case class JsonSchema(
    name: String = "entities",
    schema: Json = chatResponseSchema,
    strict: Boolean = true,
)


case class ResponseFormat(
    `type`: String = "json_schema",
    json_schema: JsonSchema = JsonSchema(),
)


val systemPrompt: String = {
  val timeStr = ZonedDateTime.now().format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
  /*
  TODO: authentication flow.

  It's better to add another action other than `callTool` and `toUser`, which opens the authentication url directly
  in the browser and use a callback url to send the instruction back to the agent without user copying anything.

  But since this is just a PoC, just keep it simple here.
   */
  s"""You are a helpful assistant.
     |
     |The current time is $timeStr.
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
     |Each tool has an optional authUrl that you can ask the user to open in the browser. If you get authentication
     | related errors when calling a tool, ask the user to open the authUrl in browser and copy the instruction back,
     | then use the instruction to try authentication again.
     |
     |Important:
     |
     |* Response only the JSON body. Never quote the response in something like json```...```.
     |* Never response to user directly without using the `toUser` field with a Json response.
     |* Only one of `callTool` and `toUser` field should be filled.
     |* Always include the `http` or `https` part for the `httpRequestEndpoint` field.
     |
     |""".stripMargin
}

case class ChatRequest(
    messages: Seq[ChatMessage],
    model: String = LLM_MODEL,
    response_format: ResponseFormat= ResponseFormat(),
)

case class ToolDef(
    httpEndpoint: String,
    openAPIPath: String,
    authUrl: Option[String] = None,
) {
  def prompt: String = {
    val authUrlPrompt = authUrl.map(url => s"Tool login URL: $url\n").getOrElse("")
    s"""----
       |Tool server endpoint: $httpEndpoint
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
    requests.get(httpEndpoint + openAPIPath).text()
  }
}

def sendLLMRequest(req: ChatRequest): Try[ChatResponse] = {
  val token = sys.env("OPENAI_API_KEY")
  val postData =  req.asJson.toString
  logToFile("Sending request ...")
  logToFile(postData)
  val r = requests.post(
    LLM_ENDPOINT + "/v1/chat/completions",
    headers = Map(
      "Authorization" -> s"Bearer $token",
      "Content-Type" -> "application/json",
    ),
    data = postData,
    readTimeout = 600000,
  )
  val resText = r.text()
  logToFile("Response:")
  logToFile(resText)
  parse(resText).toTry.flatMap { j =>
    j.hcursor
      .downField("choices")
      .downArray
      .downField("message")
      .downField("content")
      .as[String]
      .toTry
      .flatMap(decode[ChatResponse](_).toTry)
  }
}

def callTool(toolParam: ToolParam): Try[String] = {
  val hostname = toolParam.httpRequestEndpoint
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
    print("User input: ")
    val userInput = readLine()
    val nextReq = req.copy(messages = req.messages :+ ChatMessage(role = "user", content = userInput))
    val res = sendLLMRequest(nextReq)
    loop(nextReq, Some(res), waitForUser = false)
  } else {
    if (lastResponse.isEmpty || lastResponse.get.isFailure) {
      println("Error: unexpected response from LLM. Exit the conversation.")
      println(lastResponse)
    } else {
      val res = lastResponse.get.get
      val resInput = ChatMessage(role = "assistant", content = res.asJson.toString)
      if (res.toUser.isDefined) {
        val nextReq = req.copy(messages = req.messages :+ resInput)
        println("Assistant: " + res.toUser.get)
        loop(nextReq, None, waitForUser = true)
      } else if (res.callTool.isDefined) {
        logToFile("Calling tool: " + res.callTool.get)
        val toolParam = res.callTool.get
        println(s"Calling tool ${toolParam.httpRequestEndpoint}/${toolParam.httpRequestPath} ...")
        val toolOutput = callTool(toolParam).toString
        val nextReq = req.copy(messages = req.messages :+ resInput :+ ChatMessage(role = "developer", s"Tool Result:\n$toolOutput"))
        val nextRes = sendLLMRequest(nextReq)
        logToFile("Tool Result: " + toolOutput)
        loop(nextReq, Some(nextRes), waitForUser = false)
      }
    }
  }
}

def run() = {
  val tools = Seq(
    ToolDef(httpEndpoint = "https://grpc-gateway.rssbrain.com", openAPIPath = "/swagger.json",
      authUrl = Some("http://app.rssbrain.com/login?redirect_url=/llm_auth")),
  )
  val toolsPrompt = tools.map(_.prompt).mkString("\n")

  val req = ChatRequest(
    messages = Seq(
      ChatMessage(role = "system", content = systemPrompt),
      ChatMessage(role = "developer", content =
        s"""
          |
          |Here are the OpenAPI definition of the tools:
          |
          |$toolsPrompt
          |
          |""".stripMargin),
    ),
  )
  loop(req, None, waitForUser = true)
  logFile.close()
}


run()