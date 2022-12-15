package DSL

import Graph._
import sttp.client3.{RequestT, Identity, Response, SttpBackend, HttpURLConnectionBackend}

def request[R, X](req: => RequestT[Identity, Either[X, R], Any]): Model[Response[Either[X, R]], RequestError] = Model.Request(_ => req)
def request[R, X](req: => RequestT[Identity, Either[X, R], Any], code: String): Model[R, Error] =
  request(req) >> { response =>
    if (response.code.toString == code)
      yieldValue(response.body.asInstanceOf[Right[_, R]].value)
    else
      error(CodeError(s"Expected code ${code}, got ${response.code.toString}"))
  }

def failedRequest[R, X](req: => RequestT[Identity, Either[R, X], Any]): Model[Response[Either[R, X]], RequestError] = Model.FailedRequest(_ => req)
def failedRequest[R, X](req: => RequestT[Identity, Either[R, X], Any], code: String): Model[R, Error] =
  failedRequest(req) >> { response =>
    if (response.code.toString == code)
      yieldValue(response.body.asInstanceOf[Left[R, _]].value)
    else
      error(CodeError(s"Expected code ${code}, got ${response.code.toString}"))
  }

inline def assertTrue[R](data: R, inline cond: Boolean): Model[R, AssertionError] = {
  val condStr = inspect(cond)
  Model.AssertTrue(data, cond, condStr)
}
def choose[R](ms: => Model[R, Error]*): Model[R, Error] =
  val models = ms.map(m => () => m)
  Model.Choose(models)
def rec(recVarToM: RecVar => Model[Unit, Error]) = Model.Rec(recVarToM)
def loop(recVar: RecVar)                              = Model.Loop(recVar)
def error[R, E](err: => E): Model[R, E]               = Model.Error(() => err)
def yieldValue[R, E](v: => R): Model[R, E]            = Model.YieldValue(() => v)

def run[R](model: Model[R, Error]): Result[R, Error] =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  val (res, recs, logs)                   = eval(model, Map.empty, backend, 0, Seq.empty)
  res

def debug[R, B](model: Model[R, Error]): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  eval(model, Map.empty, backend, 0, Seq.empty)

def runT[R, B](model: Model[R, Error]): Result[R, Error] =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  val (res, recs, logs)                   = evalT[R, B](model.asInstanceOf[Model[Any, Error]], Map.empty, backend, 0, Seq.empty, Seq.empty)
  res

def debugT[R, B](model: Model[R, Error]): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  evalT[R, B](model.asInstanceOf[Model[Any, Error]], Map.empty, backend, 0, Seq.empty, Seq.empty)



// Graphing
inline def toMg[R, E](inline model: Model[R, E]): ModelGraph = modelToGraph[R, E](model)

private def edgeToGraphviz(edge: Edge): String =
  edge match
    case (s, label, e) => s"${s} -> ${e} [ label = \"${label.replaceAll("\"","")}\"];"

private def edgesToGraphviz(mg: ModelGraph): String =
  mg.toList match
    case Nil         => ""
    case (e :: rest) => s"${edgeToGraphviz(e)}\n${edgesToGraphviz(rest.toSet)}"

def mgToGraphvizStr(mg: ModelGraph): String = 
  "digraph finite_state_machine {\n" +
  "rankdir=T;\n" +
  "size=\"200\"\n" +
  "node [shape = circle];\n"+
  edgesToGraphviz(mg) +
  "}"
