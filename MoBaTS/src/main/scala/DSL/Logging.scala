package DSL

import Console.{RESET, UNDERLINED, RED}
import sttp.client3.{RequestT, Identity}
import scala.compiletime.ops.boolean

enum Log:
  case RequestLog[X, R](request: RequestT[Identity, Either[X, R], Any], color: String = "")
  case RecursionLog(recVar: RecVar, recVars: Set[RecVar])
  case GeneralLog(mgs: String)
  override def toString(): String = this match
    case RequestLog(request, color)    => prettyPrintRequest(request, color)
    case RecursionLog(recVar, recVars) => s"Recursion variable ID: ${recVars}\nCurrent variable set: ${recVars}"
    case GeneralLog(msg)               => msg

private def underlineString(str: String, color: String = ""): String = s"${RESET}${color}${UNDERLINED}${str}${RESET}"

private def prettyPrintRequest[X, R](request: RequestT[Identity, Either[X, R], Any], color: String): String =
  s"${color}Request Log${RESET}:\n  " +
    s"${underlineString("Method", color)}: ${color}${request.method.toString()}${RESET}\n  " +
    s"${underlineString("URI", color)}: ${color}${request.uri}${RESET}\n  " +
    s"${underlineString("Headers", color)}: ${color}${request.headers}${RESET}\n  " +
    s"${underlineString("Body", color)}: ${color}${request.body}${RESET}\n  " +
    s"${underlineString("curl command", color)}: ${color}${request.toCurl}${RESET}"
