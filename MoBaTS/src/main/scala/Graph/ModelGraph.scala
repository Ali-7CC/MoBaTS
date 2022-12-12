package Graph

import DSL.*
import scala.quoted.*
import sttp.client3.{RequestT, Identity, Response}
import scala.util.matching.Regex
import java.util.regex.Matcher

type Node = Int
type Edge = (Node, String, Node)
type ModelGraph = Set[Edge]

def modelToGraphImpl2[R: Type, E: Type](startNode: Node, modelExpr: Expr[Model[R, E]], endNode: Node, recMap: Map[String, Int])(using Quotes): Tuple2[ModelGraph, Set[Node]] =
  import quotes.reflect.*
  modelExpr match
    case '{ end($_) } =>
      val mg: ModelGraph = Set((startNode, "END", endNode))
      (mg, Set(endNode))

    case '{
          type r2
          ($model: Model[`r2`, E]) >> ($cont: `r2` => Model[R, E])
        } =>
      val (firstMg, firstExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2(startNode, model, endNode, recMap)
      val contEntryNode: Node = if (firstExitNodes.size <= 1) then firstExitNodes.max else firstExitNodes.max + 1
      val connectors: ModelGraph = if (firstExitNodes.size <= 1) then Set.empty else firstExitNodes.map(n => (n, "", firstExitNodes.max + 1))
      val (contMg, contExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2[R, E](contEntryNode, contToExpr[`r2`, R, E](cont), contEntryNode + 1, recMap)
      (firstMg union connectors union contMg, contExitNodes)

    case '{
          type x
          type r2
          request($f: RequestT[Identity, Either[`x`, `r2`], Any]): Model[Response[Either[`x`, `r2`]], RequestError]
        } =>
      val mg: ModelGraph = Set((startNode,  s"${requestToLabel[`x`, `r2`](f)}", endNode))
      (mg, Set(endNode))

    case '{
          type x
          type r2
          request($f: RequestT[Identity, Either[`x`, `r2`], Any], $code: String)
        } =>
      val mg: ModelGraph = Set((startNode, s"${requestToLabel[`x`, `r2`](f)}. Expecting code ${code.valueOrAbort}", endNode))
      (mg, Set(endNode))

    case '{
          type x
          type r2
          failedRequest($f: RequestT[Identity, Either[`x`, `r2`], Any], $code: String)
        } =>
      val mg: ModelGraph = Set((startNode,  s"${requestToLabel[`x`, `r2`](f)}. Expecting code ${code.valueOrAbort}", endNode))
      (mg, Set(endNode))

    case '{ rec($recV: String, $e) } =>
      val newMap = recMap + (recV.valueOrAbort -> startNode)
      modelToGraphImpl2(startNode, e, endNode, newMap)

    case '{ loop($recV: String) } =>
      val loopNode = recMap.get(recV.valueOrAbort).get
      val mg: ModelGraph = Set((startNode, "", loopNode))
      (mg, Set.empty)

    case '{
          choose(${ Varargs[Model[R, Error]](es) }: _*)
        } =>
      val sorted = es.sortBy(e => modelExprSize(e))
      val i: Tuple2[ModelGraph, Set[Node]] = (Set.empty, Set(startNode))
      val (mg, exitNodes) = sorted.foldRight(i)((expr, acc) => {
        val res = modelToGraphImpl2(startNode, expr, acc._2.max + 1, recMap); val mg1 = res._1; val mg2 = acc._1; val exitNodes = res._2 union acc._2; ((mg1 union mg2), exitNodes)
      })
      (mg, exitNodes diff Set(startNode))

    case _ => throw new MatchError("Could not match expression with structure:\n" + modelExpr.show + "\n And tree:\n" + modelExpr.asTerm.show(using Printer.TreeStructure))

def modelExprSize[R, E](modelExpr: Expr[Model[R, E]])(using Quotes): Int =
  val regex: Regex = ".>>".r
  regex.findAllIn(modelExpr.show).size

def recToExpr[E: Type](cont: Expr[RecVar => Model[Unit, E]])(using Quotes): Expr[Model[Unit, E]] =
  import quotes.reflect.*
  val tree: Term = cont.asTerm
  val contBody = tree match
    case Block(List(DefDef(_, _, _, Some(block))), _) => block
    case _                                            => throw new MatchError("Could not parse continuation block" + tree.show(using Printer.TreeStructure))
  val contTerm = contBody match
    case Block(_, expr) => expr
    case _              => throw new MatchError("Could not parse continuation body" + tree.show(using Printer.TreeStructure))
  val contExpr: Expr[Model[Unit, E]] = contTerm.asExprOf[Model[Unit, E]]
  contExpr

def contToExpr[R2: Type, R: Type, E: Type](cont: Expr[R2 => Model[R, E]])(using Quotes): Expr[Model[R, E]] =
  import quotes.reflect.*
  val tree: Term = cont.asTerm
  val contBody = tree match
    case Block(List(DefDef(_, _, _, Some(block))), _) => block
    case _                                            => throw new MatchError("Could not parse continuation block" + tree.show(using Printer.TreeStructure))
  val contTerm = contBody match
    case Block(_, expr) => expr
    case _              => throw new MatchError("Could not parse continuation body" + tree.show(using Printer.TreeStructure))
  val contExpr: Expr[Model[R, E]] = contTerm.asExprOf[Model[R, E]]
  contExpr

def requestToLabel[X: Type, R: Type](request: Expr[RequestT[Identity, Either[X, R], Any]])(using Quotes): String =
  import quotes.reflect.*
  val requestTree: Term = request.asTerm
  requestTree match
    case Apply(Select(Apply(Select(Ident(api), "apply"), _), endpoint), _) => s"request to ${api}.${endpoint}"
    case Block(List(ValDef(_,_, Some(Apply(Select(Ident(api), _), _)))), Apply(Select(_, endpoint), _)) => s"request to ${api}.${endpoint}"
    case _ => throw new MatchError("Could not parse request tree:\n" + requestTree.show(using Printer.TreeStructure))
