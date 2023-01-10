package DSL

import sttp.client3.{RequestT, Identity, Response}

/**
 * R: The desired data/return type that results from interpreting the model 
 * E: Error type (e.g. RequestError, CodeError, etc..) 
 * X: Response exceptions (from the sttp library)
 */
enum Model[+R, +E]:
  case Request[O, X, E](req: () => RequestT[Identity, Either[X, O], Any])       extends Model[Response[Either[X, O]], E]
  case FailedRequest[O, X, E](req: () => RequestT[Identity, Either[O, X], Any]) extends Model[Response[Either[O, X]], E]
  case Sequence[R2, R, E](firstModel: Model[R2, E], cont: R2 => Model[R, E])    extends Model[R, E]
  case AssertTrue[R, E](data: R, cond: Boolean, condStr: String)                extends Model[R, E]
  case Choose[R, E](models: Seq[() => Model[R, E]])                             extends Model[R, E]
  case Rec(recVar: RecVar => Model[Unit, E])                                    extends Model[Unit, E]
  case Loop(recVar: RecVar)                                                     extends Model[Unit, E]
  case Error[R, E](err: () => E)                                                extends Model[R, E]
  case YieldValue[R, E](v: () => R)                                             extends Model[R, E]
  def >>[R2, E2 >: E](cont: R => Model[R2, E2]): Model[R2, E2] = Model.Sequence(this, cont)

sealed trait Error(msg: String)
case class RequestError(msg: String)   extends Error(msg: String)
case class CodeError(msg: String)      extends Error(msg: String)
case class AssertionError(msg: String) extends Error(msg: String)
case class GeneralError(msg: String)   extends Error(msg: String)

sealed abstract class RecVar
private case class RecVarImpl(name: String) extends RecVar
