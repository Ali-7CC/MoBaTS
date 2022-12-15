package DSL

import Console.{RESET, UNDERLINED}
import sttp.client3.{RequestT, Identity}

sealed trait Error(msg: String)
case class RequestError(msg: String) extends Error(msg: String)
case class CodeError(msg: String) extends Error(msg: String)
case class AssertionError(msg: String) extends Error(msg: String)
case class GeneralError(msg: String) extends Error(msg: String)

def underlineString(str: String): String = s"${RESET}${UNDERLINED}${str}${RESET}"

def prettyPrintRequest[X, R](request: RequestT[Identity, Either[X, R], Any]): String =
  "Request Log:\n  " +
    s"${underlineString("Method")}: ${request.method}\n  " +
    s"${underlineString("URI")}: ${request.uri}\n  " +
    s"${underlineString("Headers")}: ${request.headers}\n  " +
    s"${underlineString("Body")}: ${request.body}\n  " +
    s"${underlineString("curl command")}: ${request.toCurl}"

enum Log:
  case RequestLog[X, R](request: RequestT[Identity, Either[X, R], Any])
  case RecursionLog(recVar: RecVar, recVars: Set[RecVar])
  case GeneralLog(mgs: String)
  override def toString(): String = this match
    case RequestLog(request)           => prettyPrintRequest(request)
    case RecursionLog(recVar, recVars) => s"Recursion variable ID: ${recVars}\nCurrent variable set: ${recVars}"
    case GeneralLog(msg)               => msg