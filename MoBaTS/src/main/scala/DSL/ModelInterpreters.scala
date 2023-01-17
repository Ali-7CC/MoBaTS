package DSL

import sttp.client3.{RequestT, Identity, Response, SttpBackend, HttpURLConnectionBackend}
import scala.util.Random
import Console.{RED, RESET}

enum Result[R, E <: Error]:
  case Success(value: R)
  case Failure(value: E)

private def eval[R](
  model: Model[R, Error],
  recMap: Map[RecVar, Model[Unit, Error]],
  backend: SttpBackend[Identity, Any],
  recCounter: Int,
  logs: Seq[Log]
): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  model match
    // BASE CASES
    case Model.Request(req) =>
      val request    = req()
      val requestLog = Log.RequestLog(request)
      val newLogs    = logs :+ requestLog
      val response   = request.send(backend)
      response.body match
        case Right(_) => (Result.Success(response), recMap, newLogs)
        case Left(x)  => (Result.Failure(RequestError(s"${RED}Request failed with exception: ${x}.${RESET}\n ${Log.RequestLog(request, RED)}\n")), recMap, newLogs)

    case Model.FailedRequest(f) =>
      val request    = f()
      val requestLog = Log.RequestLog(request)
      val newLogs    = logs :+ requestLog
      val response   = request.send(backend)
      response.body match
        case Left(_)  => (Result.Success(response), recMap, newLogs)
        case Right(_) => (Result.Failure(RequestError(s"${RED}Request was expected to fail, but didn't${RESET}\n ${Log.RequestLog(request, RED)}")), recMap, newLogs)

    case Model.AssertTrue(data, cond, condStr) =>
      if cond then
        val newLogs = logs :+ Log.GeneralLog(s"The condition \"${condStr}\" was evaluated to true")
        (Result.Success(data), recMap, newLogs)
      else
        val newLogs = logs :+ Log.GeneralLog(s"The condition \"${condStr}\" was evaluated to false")
        (Result.Failure(AssertionError(s"${RED}The condition \"${condStr}\" is false${RESET}")), recMap, newLogs)

    case Model.YieldValue(v) =>
      val newLogs = logs :+ Log.GeneralLog(s"YieldValue model with value : \"${v()}\"")
      (Result.Success(v()), recMap, newLogs)

    case Model.YieldError(err) =>
      val newLogs = logs :+ Log.GeneralLog(s"Error model with value : \"${err()}\"")
      (Result.Failure(err()), recMap, newLogs)

    // RECURSIVE CASES
    case Model.Sequence(firstModel, cont) =>
      val (res, recs, newLogs) = eval(firstModel, recMap, backend, recCounter, logs)
      res match
        case Result.Success(v)   => eval(cont(v), recs, backend, recCounter, newLogs)
        case Result.Failure(err) => (Result.Failure(err), recs, newLogs)

    case Model.Choose(ms) =>
      val length  = ms.length
      val die     = Random.nextInt(length)
      val newLogs = logs :+ Log.GeneralLog(s"Model number ${die + 1} was chosen")
      eval(ms(die)(), recMap, backend, recCounter, newLogs)

    case Model.Rec(recVarToM) =>
      val recVar    = RecVarImpl(s"rec depth: ${recCounter + 1}")
      val m         = recVarToM(recVar)
      val newRecMap = recMap + (recVar -> m)
      val newLogs   = logs :+ Log.RecursionLog(recVar, newRecMap.keySet)
      eval(m, newRecMap, backend, recCounter + 1, newLogs)

    case Model.Loop(recVar) =>
      val e = recMap.get(recVar)
      e match
        case Some(m) =>
          val newLogs = logs :+ Log.GeneralLog(s"Successful looping back to the model with recVar: \"${recVar}\"")
          eval(m, recMap, backend, recCounter, newLogs)
        case None =>
          val newLogs = logs :+ Log.GeneralLog(s"Could not loop back to model with recVar: \"${recVar}\"")
          (Result.Failure(GeneralError(s"${RED}Internal error. See logs for more details.${RESET}")), recMap, newLogs)

@scala.annotation.tailrec
private def evalT[R](
  model: Model[Any, Error],
  recMap: Map[RecVar, Model[Unit, Error]],
  backend: SttpBackend[Identity, Any],
  recCounter: Int,
  logs: Seq[Log],
  conts: Seq[Any => Model[Any, Error]]
): (Result[R, Error], Map[RecVar, Model[Unit, Error]], Seq[Log]) =
  model match
    // BASE CASES
    case Model.Request(req) =>
      val request    = req()
      val requestLog = Log.RequestLog(request)
      val newLogs    = logs :+ requestLog
      val response   = request.send(backend)
      response.body match
        case Right(_) =>
          if (conts.isEmpty)
            (Result.Success(response.asInstanceOf[R]), recMap, newLogs)
          else
            evalT(conts(0)(response), recMap, backend, recCounter, newLogs, conts.drop(1))
        case Left(x) => (Result.Failure(RequestError(s"${RED}Request failed with exception: ${x}.${RESET}\n ${Log.RequestLog(request, RED)}")), recMap, newLogs)

    case Model.FailedRequest(req) =>
      val request    = req()
      val requestLog = Log.RequestLog(request)
      val newLogs    = logs :+ requestLog
      val response   = request.send(backend)
      response.body match
        case Left(_) =>
          if (conts.isEmpty)
            (Result.Success(response.asInstanceOf[R]), recMap, newLogs)
          else
            evalT(conts(0)(response), recMap, backend, recCounter, newLogs, conts.drop(1))
        case Right(_) => (Result.Failure(RequestError(s"${RED}Request was expected to fail, but didn't${RESET}\n ${Log.RequestLog(request, RED)}")), recMap, newLogs)

    case Model.AssertTrue(m, cond, condStr) =>
      if (cond)
        val newLogs = logs :+ Log.GeneralLog(s"The condition \"${condStr}\" was evaluated to true")
        if (conts.isEmpty)
          (Result.Success(m.asInstanceOf[R]), recMap, newLogs)
        else
          evalT(conts(0)(m), recMap, backend, recCounter, newLogs, conts.drop(1))
      else
        val newLogs = logs :+ Log.GeneralLog(s"The condition \"${condStr}\" was evaluated to false")
        (Result.Failure(AssertionError(s"${RED}The condition \"${condStr}\" is false${RESET}")), recMap, newLogs)

    case Model.YieldValue(v) =>
      val newLogs = logs :+ Log.GeneralLog(s"YieldValue Model with value : \"${v()}\"")
      if (conts.isEmpty)
        (Result.Success(v().asInstanceOf[R]), recMap, newLogs)
      else
        evalT(conts(0)(v()), recMap, backend, recCounter, newLogs, conts.drop(1))

    case Model.YieldError(v) =>
      val newLogs = logs :+ Log.GeneralLog(s"Error Model with value : \"${v()}\"")
      (Result.Failure(v()), recMap, newLogs)

    // RECURSIVE CASES
    case Model.Sequence(first, cont) =>
      evalT(first, recMap, backend, recCounter, logs, cont.asInstanceOf[Any => Model[Any, Error]] +: conts)

    case Model.Choose(ms) =>
      val length  = ms.length
      val die     = Random.nextInt(length)
      val newLogs = logs :+ Log.GeneralLog(s"Model number ${die + 1} was chosen")
      evalT(ms(die)(), recMap, backend, recCounter, newLogs, conts)

    case Model.Rec(recVarToM) =>
      val recVar    = RecVarImpl(s"rec depth: ${recCounter + 1}")
      val m         = recVarToM(recVar)
      val newRecMap = recMap + (recVar -> m)
      val newLogs   = logs :+ Log.RecursionLog(recVar, newRecMap.keySet)
      evalT(m, newRecMap.asInstanceOf[Map[RecVar, Model[Unit, Error]]], backend, recCounter + 1, newLogs, conts)

    case Model.Loop(recVar) =>
      val m = recMap.get(recVar)
      m match
        case Some(m) =>
          val newLogs = logs :+ Log.GeneralLog(s"Successful looping back to the model with recVar: \"${recVar}\"")
          evalT(m, recMap, backend, recCounter, newLogs, conts)
        case None =>
          val newLogs = logs :+ Log.GeneralLog(s"Could not loop back to model with recVar: \"${recVar}\"")
          (Result.Failure(GeneralError(s"${RED}Internal error. See logs for more details.${RESET}")), recMap, newLogs)
