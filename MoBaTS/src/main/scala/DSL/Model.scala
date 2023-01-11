package DSL

import sttp.client3.{RequestT, Identity, Response}

/**
 * R: The desired data/return type that results from interpreting the model E:
 * Error type (e.g. RequestError, CodeError, etc..) X: Response exceptions (from
 * the sttp library)
 */
enum Model[+R, +E <: DSL.Error]:
  case Request[O, X, E <: DSL.Error](req: () => RequestT[Identity, Either[X, O], Any])                      extends Model[Response[Either[X, O]], E]
  case FailedRequest[O, X, E <: DSL.Error](req: () => RequestT[Identity, Either[O, X], Any])                extends Model[Response[Either[O, X]], E]
  case Sequence[R2, R, E <: DSL.Error, E2 <: DSL.Error](firstModel: Model[R2, E2], cont: R2 => Model[R, E]) extends Model[R, E]
  case AssertTrue[R, E <: DSL.Error](data: R, cond: Boolean, condStr: String)                               extends Model[R, E]
  case Choose[R, E <: DSL.Error](models: Seq[() => Model[R, E]])                                            extends Model[R, E]
  case Rec(recVar: RecVar => Model[Unit, E])                                                                extends Model[Unit, E]
  case Loop(recVar: RecVar)                                                                                 extends Model[Unit, E]
  case Error[R, E <: DSL.Error](err: () => E)                                                               extends Model[R, E]
  case YieldValue[R, E <: DSL.Error](v: () => R)                                                            extends Model[R, E]
  def >>[R2, E2 <: DSL.Error](cont: R => Model[R2, E2]): Model[R2, E2] = Model.Sequence(this, cont)

sealed trait Error(msg: String)
case class RequestError(msg: String)   extends Error(msg: String)
case class CodeError(msg: String)      extends Error(msg: String)
case class AssertionError(msg: String) extends Error(msg: String)
case class GeneralError(msg: String)   extends Error(msg: String)

sealed abstract class RecVar
private case class RecVarImpl(name: String) extends RecVar
