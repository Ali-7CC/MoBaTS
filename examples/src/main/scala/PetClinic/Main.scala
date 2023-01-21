package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*


@main def main: Unit =
  // Chosen model (some examples available in Models.scala)
  inline def model = ownerApiModel

  // Graphing the model
  val mg = modelToGraph(model)
  val graphvizStr = mgToGraphvizStr(mg)
  println(s"Graphvis string:\n\n${graphvizStr}")

  // Running the model
  println(run(model))

  // Running the test in debug mode (verbose logs:)
  // val output = debug(model)
  // output match
  //   case (Result.Success(value), _, logs) =>
  //     println("Successful run with value:")
  //     println(value)
  //     println("Log stack:")
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
  //   case (Result.Failure(error), _, logs) =>
  //     println("Failed run with error:")
  //     println(error)
  //     println("Log trace:")
  //     for log <- logs do
  //       println(log)
  //       println("----------------")


  // Running the model n times, and printing all the errors
  // print(runner(model, Set.empty, 0, 100))