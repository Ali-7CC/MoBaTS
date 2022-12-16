package Graph

import DSL.*
import scala.quoted.*
import sttp.client3.{RequestT, Identity, Response}
import scala.util.matching.Regex
import java.util.regex.Matcher

type Node       = Int
type Edge       = (Node, String, Node)
type ModelGraph = Set[Edge]

def modelToGraphImpl2[R: Type, E: Type](startNode: Node, modelExpr: Expr[Model[R, E]], endNode: Node, recMap: Map[String, Int])(using Quotes): Tuple2[ModelGraph, Set[Node]] =
  import quotes.reflect.*
  modelExpr match
    case '{ yieldValue($_) } => (Set.empty, Set(startNode))

    case '{
          type r2
          ($model: Model[`r2`, E]) >> ($cont: `r2` => Model[R, E])
        } =>
      val (firstMg, firstExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2(startNode, model, endNode, recMap)
      val contEntryNode: Node                                = if (firstExitNodes.size <= 1) then firstExitNodes.max else firstExitNodes.max + 1
      val connectors: ModelGraph                             = if (firstExitNodes.size <= 1) then Set.empty else firstExitNodes.map(n => (n, "", firstExitNodes.max + 1))
      val (contMg, contExitNodes): (ModelGraph, Set[Node])   = modelToGraphImpl2[R, E](contEntryNode, contToExpr[`r2`, R, E](cont), contEntryNode + 1, recMap)
      (firstMg union connectors union contMg, contExitNodes)

    case '{
          type x
          type r2
          request($f: RequestT[Identity, Either[`x`, `r2`], Any]): Model[Response[Either[`x`, `r2`]], RequestError]
        } =>
      val (api, endpoint) = parseRequest[`x`, `r2`](f)
      val mg: ModelGraph = Set((startNode, s"!${api}.${endpoint}", endNode))
      (mg, Set(endNode + 1))

    case '{
          type x
          type r2
          request($f: RequestT[Identity, Either[`x`, `r2`], Any], $code: String)
        } =>
      val (api, endpoint) = parseRequest[`x`, `r2`](f)
      val mg: ModelGraph = Set((startNode, s"!${api}.${endpoint}", endNode), ((endNode, s"?${code.valueOrAbort}", endNode + 1)))
      (mg, Set(endNode + 1))

    case '{
          type x
          type r2
          failedRequest($f: RequestT[Identity, Either[`x`, `r2`], Any], $code: String)
        } =>
      val (api, endpoint) = parseRequest[`x`, `r2`](f)
      val mg: ModelGraph = Set((startNode, s"!${api}.${endpoint}", endNode), ((endNode, s"?${code.valueOrAbort}", endNode + 1)))
      (mg, Set(endNode + 1))

    case '{ rec($recVarToM: RecVar => Model[Unit, Error]) } =>
      val (recVarStr, mExpr) = parseRecVarToM(recVarToM)
      if recMap.contains(recVarStr) then report.errorAndAbort(s"Recursion variable ${recVarStr} is already used. Please use a unique recursion variable.")
      val mg1 = Set((startNode, s"rec(${recVarStr})", endNode))
      val newRecMap = recMap + (recVarStr -> (endNode))
      val (mg2, exitNodes) = modelToGraphImpl2(endNode, mExpr, endNode + 1, newRecMap)
      (mg1 union mg2, exitNodes)

    case '{ loop($recVar: RecVar) } =>
      val recVarStr = recVarToStr(recVar)
      val loopNode       = recMap.get(recVarStr).get
      val mg: ModelGraph = Set((startNode, s"loop(${recVarStr})", loopNode))
      (mg, Set.empty)

    case '{
          choose(${ Varargs[Model[R, Error]](es) }: _*)
        } =>
      val i: Tuple2[ModelGraph, Set[Node]] = (Set.empty, Set.empty)
      val (mg, exitNodes) = es.foldRight(i) { (expr, acc) =>
        val res       = modelToGraphImpl2(startNode, expr, if acc._1.isEmpty then startNode + 1 else mgMaxNode(acc._1) + 1, recMap);
        val newMg = res._1 union acc._1
        val newExitNodes = res._2 union acc._2;
        (newMg, newExitNodes)
      }
      (mg, exitNodes)

    case '{
          val condStr: String = $x; $body(condStr): Model[R, E]
        } =>
      val mg: ModelGraph = Set((startNode, s"AssertTrue with condition: ${x.valueOrAbort}", endNode))
      (mg, Set(endNode))

    case '{
          if $cond then $thenBranch else $elseBranch
        } =>
      val thenConnector: ModelGraph = Set((startNode, cond.show, endNode))
      val (thenMg, thenExitNodes): (ModelGraph, Set[Node])  = modelToGraphImpl2(endNode, thenBranch, endNode + 1, recMap)
      val elseConnector: ModelGraph = Set((startNode, s"¬(${cond.show})", thenExitNodes.max + 1))
      val (elseMg, elseExitNodes): (ModelGraph, Set[Node])  = modelToGraphImpl2(thenExitNodes.max + 1, elseBranch, thenExitNodes.max + 2, recMap)
      (thenConnector union thenMg union elseConnector union elseMg, thenExitNodes union elseExitNodes)

    case _ => throw new MatchError("Could not match expression with structure:\n" + modelExpr.show + "\n And tree:\n" + modelExpr.asTerm.show(using Printer.TreeStructure))


def recVarToStr(recVar: Expr[RecVar])(using Quotes): String = 
  import quotes.reflect.*
  val recVarTree = recVar.asTerm
  recVarTree match
    case Ident(param) => param
    case _ => throw new MatchError("Could not parse recursion variable with tree:\n" + recVarTree.show(using Printer.TreeStructure))

def modelExprSize[R, E](modelExpr: Expr[Model[R, E]])(using Quotes): Int =
  val regex: Regex = ".>>".r
  regex.findAllIn(modelExpr.show).size

def mgMaxNode(mg: ModelGraph): Int = mg match
  case mg if mg.size < 1 => 0
  case _                 => mg.map((a, b, c) => a.max(c)).max

def parseRecVarToM[E: Type](cont: Expr[RecVar => Model[Unit, E]])(using Quotes): (String, Expr[Model[Unit, E]]) =
  import quotes.reflect.*
  val tree: Term = cont.asTerm
  val (param, contBody) = tree match
    case Block(List(DefDef(_, List(TermParamClause(List(ValDef(param, _, _)))), _, Some(block))), _) => (param, block)
    case _                                            => throw new MatchError("Could not parse continuation block" + tree.show(using Printer.TreeStructure))
  val contTerm = contBody match
    case Block(_, expr) => expr
    case _              => throw new MatchError("Could not parse continuation body" + tree.show(using Printer.TreeStructure))
  val contExpr: Expr[Model[Unit, E]] = contTerm.asExprOf[Model[Unit, E]]
  (param, contExpr)

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

def parseRequest[X: Type, R: Type](request: Expr[RequestT[Identity, Either[X, R], Any]])(using Quotes): (String, String) =
  import quotes.reflect.*
  val requestTree: Term = request.asTerm
  requestTree match
    case Apply(Select(Apply(Select(Ident(api), "apply"), _), endpoint), _)                               => (api, endpoint)
    case Block(List(ValDef(_, _, Some(Apply(Select(Ident(api), _), _)))), Apply(Select(_, endpoint), _)) => (api, endpoint)
    case _                                                                                               => throw new MatchError("Could not parse request tree:\n" + requestTree.show(using Printer.TreeStructure))
