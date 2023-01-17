package DSL

import scala.quoted.*

def booleanToStringImpl(condExpr: Expr[Boolean])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(condExpr.asTerm.show(using Printer.TreeShortCode))

def modelPrinterImpl[R: Type](modelExpr: Expr[Model[R, Error]])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr("Using .show: \n\n" + modelExpr.show + "\n\n" + "Using asTerm.show:\n\n" + modelExpr.asTerm.show(using Printer.TreeStructure))
