package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api._
import org.openapitools.client.model._

@main def hello: Unit =
  inline def m = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201") >> {
    owner => if owner.firstName == "Ali" && owner.lastName == "E" then 
      request(OwnerApi().deleteOwner(owner.id.get)) else 
      request(OwnerApi().updateOwner(owner.id.get, OwnerFields("Jack", "Doe", "Street 4", "Copenhagen", "12345678")), "204")}

  val graphvizStr = mgToGraphvizStr(toMg(m))
  println(s"Graphvis string:\n\n${graphvizStr}")


  // Constructing an API model
  // inline def model =
  //   request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201") >> { owner =>
  //     rec {x =>
  //       choose(
  //         request(OwnerApi().deleteOwner(owner.id.get), "204"),
  //         request(OwnerApi().updateOwner(owner.id.get, OwnerFields("Jack", "Doe", "Street 4", "Copenhagen", "12345678")), "204")  >> { _ => loop(x) },
  //         request(OwnerApi().listOwners(), "200") >> {_ => yieldValue(())}
  //       )} >> 
  //         { _ => request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201") }}

  // // Graphing the model
  // inline def ifModel = model >> { owner => 
  //   if owner.firstName == "John" then 
  //     request(OwnerApi().updateOwner(owner.id.get, OwnerFields("Jack", "Doe", "Street 4", "Copenhagen", "12345678")), "204")
  //   else 
  //     request(OwnerApi().deleteOwner(owner.id.get), "204")
  //   }
  // val mg          = toMg(ifModel)
  // val graphvizStr = mgToGraphvizStr(mg)
  // println(s"Graphvis string:\n\n${graphvizStr}")

  // Running the model
  // println(run(model))

  // Running the test in debug mode (verbose logs:)
  // val output = debug(model)
  // output match
  //   case (Result.Success(value), recs, logs) =>
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
  //   case (Result.Failure(error), _, _) => println("ERROR"); print(error)
