package Graph

import DSL.*
import scala.quoted.*

def modelToGraphImpl[R: Type, E: Type](modelExpr: Expr[Model[R, E]])(using Quotes): Expr[ModelGraph] =
  val res: Tuple2[ModelGraph, Set[Node]] = modelToGraphImpl2[R, E](0, modelExpr, 1, Map.empty)
  Expr(res._1)
