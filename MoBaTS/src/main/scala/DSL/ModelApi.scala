package DSL

import Graph.*
import sttp.client3.{RequestT, Identity, Response, SttpBackend, HttpURLConnectionBackend}
import Console.{RED, RESET}

// Model constructors API
def request[S, X](req: => RequestT[Identity, Either[X, S], Any]): Model[Response[Either[X, S]], RequestError] = Model.Request(() => req)
def request[R, X](req: => RequestT[Identity, Either[X, R], Any], code: String): Model[R, Error] =
  request(req) >> { response =>
    if (response.code.toString == code)
      yieldValue(response.body.asInstanceOf[Right[_, R]].value)
    else
      yieldError(CodeError(s"${RED}Expected code ${code}, got ${response.code.toString}${RESET}"))
  }

def failedRequest[S, X](req: => RequestT[Identity, Either[S, X], Any]): Model[Response[Either[S, X]], RequestError] = Model.FailedRequest(() => req)
def failedRequest[R, X](req: => RequestT[Identity, Either[R, X], Any], code: String): Model[R, Error] =
  failedRequest(req) >> { response =>
    if (response.code.toString == code)
      yieldValue(response.body.asInstanceOf[Left[R, _]].value)
    else
      yieldError(CodeError(s"${RED}Expected code ${code}, got ${response.code.toString}${RESET}"))
  }

inline def assertTrue[R](data: R, inline cond: Boolean): Model[R, AssertionError] = {
  val condStr = booleanToString(cond)
  Model.AssertTrue(data, cond, condStr)
}

def choose[R](ms: => Model[R, Error]*): Model[R, Error] =
  val models = ms.map(m => () => m)
  Model.Choose(models)

def rec(recVarToM: RecVar => Model[Unit, Error]): Model[Unit, Error] = Model.Rec(recVarToM)
def loop(recVar: RecVar): Model[Unit, Error]                         = Model.Loop(recVar)
def yieldValue[R](v: => R): Model[R, Nothing]                        = Model.YieldValue(() => v)
private def yieldError(err: => Error): Model[Nothing, Error]         = Model.YieldError(() => err)

// Model interpreters API
def run[R](model: Model[R, Error]): Result[R, Error] =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  val (res, recs, logs)                   = eval(model, Map.empty, backend, 0, Seq.empty)
  res

def debug[R](model: Model[R, Error]): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  eval(model, Map.empty, backend, 0, Seq.empty)

def runT[R](model: Model[R, Error]): Result[R, Error] =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  val (res, recs, logs)                   = evalT[R](model, Map.empty, backend, 0, Seq.empty, Seq.empty)
  res

def debugT[R](model: Model[R, Error]): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  evalT[R](model, Map.empty, backend, 0, Seq.empty, Seq.empty)

// Graphing API
inline def toMg[R](inline model: Model[R, Error]): ModelGraph = modelToGraph[R](model)

private def edgeToGraphviz(edge: Edge): String =
  edge match
    case (s, label, e) => s"${s} -> ${e} [ label = \"${label.replaceAll("\"", "")}\"];"

private def edgesToGraphviz(mg: ModelGraph): String =
  mg.toList match
    case Nil         => ""
    case (e :: rest) => s"${edgeToGraphviz(e)}\n${edgesToGraphviz(rest.toSet)}"

def mgToGraphvizStr(mg: ModelGraph): String =
  "digraph finite_state_machine {\n" +
    "rankdir=T;\n" +
    "size=\"200\"\n" +
    "node [shape = circle];\n" +
    edgesToGraphviz(mg) +
    "}"
