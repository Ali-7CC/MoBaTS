package DSL

import scala.quoted.*

def inspectCode(x: Expr[Any])(using Quotes): Expr[String] =
  Expr(x.show)

def modelPrinterImpl[R: Type, E: Type](modelExpr: Expr[Model[R, E]])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr("Using .show: \n\n" + modelExpr.show + "\n\n" + "Using asTerm.show:\n\n" + modelExpr.asTerm.show(using Printer.TreeStructure))