package DSL
import Graph.*
import sttp.client3.{RequestT, Identity, Response}

sealed abstract class RecVar
private case class RecVarImpl(name: String) extends RecVar

/**
 * R: The desired data/return type that results from interpreting the model E:
 * Error type (e.g. ResponseError, CodeError, etc..) X: Response exceptions
 * (from the sttp library)
 */
enum Model[+R, +E]:
  case Sequence[R2, R, E](firstModel: Model[R2, E], cont: R2 => Model[R, E])      extends Model[R, E]
  case Request[R, X, E](req: Unit => RequestT[Identity, Either[X, R], Any])       extends Model[Response[Either[X, R]], E]
  case FailedRequest[R, X, E](req: Unit => RequestT[Identity, Either[R, X], Any]) extends Model[Response[Either[R, X]], E]
  case AssertTrue[R, E](data: R, cond: Boolean, condStr: String)                  extends Model[R, E]
  case Choose[R, E](models: Seq[() => Model[R, E]])                               extends Model[R, E]
  case Rec(recVar: RecVar => Model[Unit, E])                                      extends Model[Unit, E]
  case Loop(recVar: RecVar)                                                       extends Model[Unit, E]
  case EndLoop()                                                                  extends Model[Unit, E]
  case Error[R, E](err: () => E)                                                  extends Model[R, E]
  case YieldValue[R, E](v: () => R)                                               extends Model[R, E]
  def >>[R2, E2 >: E](cont: R => Model[R2, E2]): Model[R2, E2] = Model.Sequence(this, cont)
