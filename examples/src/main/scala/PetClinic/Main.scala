package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*
import java.time.LocalDate

@main def main: Unit =
  // Chosen model
  inline def model = ownerApiModel
  val m: Model[Owner, Error] = assertTrue(Owner.apply("dsa", "dsa", "sda", "das", "dsa", Some(1), Seq.empty), 1 == 2)

  val m2  = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "12345678")), "200")

  // Graphing the model
  // val mg          = toMg(model)
  // val graphvizStr = mgToGraphvizStr(mg)
  // println(s"Graphvis string:\n\n${graphvizStr}")

  // Running the model
  println(runT(model))

  // Running the test in debug mode (verbose logs:)
  // val output = debug(model)
  // output match
  //   case (Result.Success(value), recs, logs) =>
  //     println("Successful. Log trace:\n")
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
  //   case (Result.Failure(error), _, logs) =>
  //     println("Failed with error:\n")
  //     println(error)
  //     println("LOG TRACE:\n")
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
