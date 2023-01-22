package Graph

import DSL.*
import scala.quoted.*
import sttp.client3.{RequestT, Identity, Response}

type Node       = Int
type Edge       = (Node, String, Node)
type ModelGraph = Set[Edge]

// User-facing macro
inline def modelToGraph[R](inline model: Model[R, Error]): ModelGraph = ${ modelToGraphImpl[R]('{ model }) }

// Converting model graph to Graphvis string
def mgToGraphvizStr(mg: ModelGraph): String =
  "digraph finite_state_machine {\n" +
    "rankdir=T;\n" +
    "size=\"200\"\n" +
    "node [shape = circle];\n" +
    edgesToGraphviz(mg) +
    "}"


private def modelToGraphImpl[R: Type](modelExpr: Expr[Model[R, Error]])(using Quotes): Expr[ModelGraph] =
  val res: Tuple2[ModelGraph, Set[Node]] = modelToGraphImpl2[R](0, modelExpr, 1, Map.empty)
  Expr(res._1)

private def modelToGraphImpl2[R: Type](sourceNode: Node, modelExpr: Expr[Model[R, Error]], targetNode: Node, recMap: Map[String, Int])(using Quotes): Tuple2[ModelGraph, Set[Node]] =
  import quotes.reflect.*
  modelExpr match
    case '{ yieldValue($_) } => (Set.empty, Set(sourceNode))

    case '{
          type r2
          ($model: Model[`r2`, Error]) >> ($cont: `r2` => Model[R, Error])
        } =>
      val (firstMg, firstExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2(sourceNode, model, targetNode, recMap)
      firstExitNodes match
        case exitNodes if exitNodes.isEmpty => report.errorAndAbort("Incorrect model. Check for infinite loops.")
        case exitNodes if exitNodes.size == 1 =>
          val contEntryNode  = exitNodes.max
          val contTargetNode = mgMaxNode(firstMg) + 1
          val (contMg, contExitNodes): (ModelGraph, Set[Node]) =
            modelToGraphImpl2[R](contEntryNode, contToExpr[`r2`, R](cont), contTargetNode, recMap)
          (firstMg union contMg, contExitNodes)
        case exitNodes =>
          val contEntryNode  = mgMaxNode(firstMg) + 1
          val contTargetNode = contEntryNode + 1
          val connectorMg    = exitNodes.map(n => (n, "", contEntryNode))
          val (contMg, contExitNodes): (ModelGraph, Set[Node]) =
            modelToGraphImpl2[R](contEntryNode, contToExpr[`r2`, R](cont), contTargetNode, recMap)
          (firstMg union connectorMg union contMg, contExitNodes)

    case '{
          type x
          type r2
          request($f: RequestT[Identity, Either[`x`, `r2`], Any]): Model[Response[Either[`x`, `r2`]], RequestError]
        } =>
      val (api, endpoint) = parseRequest[`x`, `r2`](f)
      val mg: ModelGraph  = Set((sourceNode, s"!${api}.${endpoint}", targetNode))
      (mg, Set(targetNode))

    case '{
          type x
          request($req: RequestT[Identity, Either[`x`, R], Any], $code: String)
        } =>
      val (api, endpoint) = parseRequest[`x`, R](req)
      val mg: ModelGraph  = Set((sourceNode, s"!${api}.${endpoint}", targetNode), ((targetNode, s"?${code.valueOrAbort}", targetNode + 1)))
      (mg, Set(targetNode + 1))

    case '{
          type x
          type r2
          failedRequest($f: RequestT[Identity, Either[`x`, `r2`], Any], $code: String)
        } =>
      val (api, endpoint) = parseRequest[`x`, `r2`](f)
      val mg: ModelGraph  = Set((sourceNode, s"!${api}.${endpoint}", targetNode), ((targetNode, s"?${code.valueOrAbort}", targetNode + 1)))
      (mg, Set(targetNode + 1))

    case '{ rec($recVarToM: RecVar => Model[Unit, Error]) } =>
      val (recVarStr, mExpr) = parseRecVarToM(recVarToM)
      if recMap.contains(recVarStr) then report.errorAndAbort(s"Recursion variable ${recVarStr} is already used. Please use a unique recursion variable.")
      val newRecMap        = recMap + (recVarStr -> (targetNode))
      val mg1              = Set((sourceNode, s"rec(${recVarStr})", targetNode))
      val (mg2, exitNodes) = modelToGraphImpl2(targetNode, mExpr, targetNode + 1, newRecMap)
      (mg1 union mg2, exitNodes)

    case '{ loop($recVar: RecVar) } =>
      val recVarStr      = recVarToStr(recVar)
      val loopNode       = recMap.get(recVarStr).get
      val mg             = Set((sourceNode, s"loop(${recVarStr})", loopNode))
      (mg, Set.empty)

    case '{
          choose(${Varargs(es)}: _*): Model[R, Error]
        } =>
      val i: Tuple2[ModelGraph, Set[Node]] = (Set.empty, Set.empty)
      val (mg, exitNodes) = es.foldRight(i) { (expr, acc) =>
        val res          = modelToGraphImpl2(sourceNode, expr, if acc._1.isEmpty then targetNode else mgMaxNode(acc._1) + 1, recMap)
        val newMg        = res._1 union acc._1
        val newExitNodes = res._2 union acc._2
        (newMg, newExitNodes)
      }
      (mg, exitNodes)

    case '{
          val condStr: String = $x; $body(condStr): Model[R, Error]
        } =>
      val mg: ModelGraph = Set((sourceNode, s"AssertTrue: ${x.valueOrAbort}", targetNode))
      (mg, Set(targetNode))

    case '{
          if $cond then $thenBranch else $elseBranch
        } =>
      val thenConnector: ModelGraph                        = Set((sourceNode, cond.show, targetNode))
      val (thenMg, thenExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2(targetNode, thenBranch, targetNode + 1, recMap)
      val elseConnector: ModelGraph                        = Set((sourceNode, s"¬(${cond.show})", mgMaxNode(thenMg) + 1))
      val (elseMg, elseExitNodes): (ModelGraph, Set[Node]) = modelToGraphImpl2(mgMaxNode(thenMg) + 1, elseBranch, mgMaxNode(thenMg) + 2, recMap)
      (thenConnector union thenMg union elseConnector union elseMg, thenExitNodes union elseExitNodes)

    case _ => throw new MatchError("Could not match expression with structure:\n" + modelExpr.show + "\n And tree:\n" + modelExpr.asTerm.show(using Printer.TreeStructure))

private def recVarToStr(recVar: Expr[RecVar])(using Quotes): String =
  import quotes.reflect.*
  val recVarTree = recVar.asTerm
  recVarTree match
    case Ident(param) => param
    case _            => throw new MatchError("Could not parse recursion variable with tree:\n" + recVarTree.show(using Printer.TreeStructure))

private def mgMaxNode(mg: ModelGraph): Int = mg match
  case mg if mg.size < 1 => 0
  case _                 => mg.map((a, b, c) => a.max(c)).max

private def parseRecVarToM[E: Type](recVarToM: Expr[RecVar => Model[Unit, Error]])(using Quotes): (String, Expr[Model[Unit, Error]]) =
  import quotes.reflect.*
  val tree: Term = recVarToM.asTerm
  val (recVarStr, contBody) = tree match
    case Block(List(DefDef(_, List(TermParamClause(List(ValDef(recVarStr, _, _)))), _, Some(block))), _) => (recVarStr, block)
    case _                                                                                           => throw new MatchError("Could not parse continuation block" + tree.show(using Printer.TreeStructure))
  val contTerm = contBody match
    case Block(_, expr) => expr
    case _              => throw new MatchError("Could not parse continuation body" + tree.show(using Printer.TreeStructure))
  val contExpr: Expr[Model[Unit, Error]] = contTerm.asExprOf[Model[Unit, Error]]
  (recVarStr, contExpr)

private def contToExpr[R2: Type, R: Type](cont: Expr[R2 => Model[R, Error]])(using Quotes): Expr[Model[R, Error]] =
  import quotes.reflect.*
  val tree: Term = cont.asTerm
  val contBody = tree match
    case Block(List(DefDef(_, _, _, Some(block))), _) => block
    case _                                            => throw new MatchError("Could not parse continuation block" + tree.show(using Printer.TreeStructure))
  val contTerm = contBody match
    case Block(_, expr) => expr
    case _              => throw new MatchError("Could not parse continuation body" + tree.show(using Printer.TreeStructure))
  val contExpr: Expr[Model[R, Error]] = contTerm.asExprOf[Model[R, Error]]
  contExpr

private def parseRequest[X: Type, R: Type](request: Expr[RequestT[Identity, Either[X, R], Any]])(using Quotes): (String, String) =
  import quotes.reflect.*
  val requestTree: Term = request.asTerm
  requestTree match
    case Apply(Select(Apply(Select(Ident(api), "apply"), _), endpoint), _)                               => (api, endpoint)
    case Block(List(ValDef(_, _, Some(Apply(Select(Ident(api), _), _)))), Apply(Select(_, endpoint), _)) => (api, endpoint)
    case _                                                                                               => throw new MatchError("Could not parse request tree:\n" + requestTree.show(using Printer.TreeStructure))

private def edgeToGraphviz(edge: Edge): String =
  edge match
    case (s, label, e) => s"${s} -> ${e} [ label = \"${label.replaceAll("\"", "")}\"];"

private def edgesToGraphviz(mg: ModelGraph): String =
  mg.toList match
    case Nil         => ""
    case (e :: rest) => s"${edgeToGraphviz(e)}\n${edgesToGraphviz(rest.toSet)}"

