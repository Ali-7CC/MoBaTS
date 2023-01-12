package DSL

import sttp.client3.{RequestT, Identity, Response}

/**
 * R: The result from interpreting the model.
 * E: Error type (e.g. RequestError, CodeError, etc..).
 * S: Response type (from sttp).
 * X: Response exception type (from sttp).
 */
enum Model[+R, +E <: Error]:
  case Request[X, S, E <: Error](req: () => RequestT[Identity, Either[X, S], Any])                  extends Model[Response[Either[X, S]], E]
  case FailedRequest[X, S, E <: Error](req: () => RequestT[Identity, Either[S, X], Any])            extends Model[Response[Either[S, X]], E]
  case Sequence[R, R2, E <: Error, E2 <: Error](firstModel: Model[R2, E2], cont: R2 => Model[R, E]) extends Model[R, E]
  case AssertTrue[R, E <: Error](data: R, cond: Boolean, condStr: String)                           extends Model[R, E]
  case Choose[R, E <: Error](models: Seq[() => Model[R, E]])                                        extends Model[R, E]
  case Rec(recVar: RecVar => Model[Unit, E])                                                        extends Model[Unit, E]
  case Loop(recVar: RecVar)                                                                         extends Model[Unit, E]
  case YieldValue[R, E <: Error](v: () => R)                                                        extends Model[R, E]
  case YieldError[R, E <: Error](err: () => E)                                                      extends Model[R, E]
  def >>[R2, E2 <: Error](cont: R => Model[R2, E2]): Model[R2, E2] = Model.Sequence(this, cont)

sealed trait Error(msg: String)
case class RequestError(msg: String)   extends Error(msg: String)
case class CodeError(msg: String)      extends Error(msg: String)
case class AssertionError(msg: String) extends Error(msg: String)
case class GeneralError(msg: String)   extends Error(msg: String)

sealed abstract class RecVar
private case class RecVarImpl(name: String) extends RecVar
