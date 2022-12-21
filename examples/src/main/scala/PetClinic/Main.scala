package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*
import java.time.LocalDate

@main def main: Unit =
  // Chosen model
  inline def model = modelA

  // Graphing the model
  val mg          = toMg(model)
  val graphvizStr = mgToGraphvizStr(mg)
  println(s"Graphvis string:\n\n${graphvizStr}")

  // Running the model
  // println(run(model))

  // Running the test in debug mode (verbose logs:)
  // val output = debug(model)
  // output match
  //   case (Result.Success(value), recs, logs) =>
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
  //   case (Result.Failure(error), _, logs) =>
  //     print(error)
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
