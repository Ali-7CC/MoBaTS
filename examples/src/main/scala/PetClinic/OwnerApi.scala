package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api._
import org.openapitools.client.model._

inline def step1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
inline def step2 = step1 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner => assertTrue(retrievedOwner, owner == retrievedOwner) } }
inline def step3 = step2 >> { owner =>
  request(OwnerApi().updateOwner(owner.id.get, OwnerFields("updatedName", "updatedLastName", "Updated Address", "Updated City", "12345678")), "204") >> { _ => yieldValue(owner) }
}
inline def step4 = step3 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") }
inline def step5 = step4 >> { owner => request(OwnerApi().listOwners(), "200") >> { owners => assertTrue(owners, owners.contains(owner)) >> { _ => yieldValue(owner) } } }
inline def step6 = step5 >> { owner => request(OwnerApi().listOwners(Some(owner.lastName)), "200") >> { owners => assertTrue(owners, owners.contains(owner)) >> { _ => yieldValue(owner) } } }
inline def step7 = step6 >> { owner => request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue(owner) } }
inline def step8 = step7 >> { owner => failedRequest(OwnerApi().getOwner(owner.id.get), "404") >> { _ => yieldValue(owner) } }
inline def step9 = step8 >> { owner =>
  failedRequest(OwnerApi().updateOwner(owner.id.get, OwnerFields("updatedName", "updatedLastName", "Updated Address", "Updated City", "12345678")), "404") >> { _ => yieldValue(owner) }
}
inline def step10       = step9 >> { owner => failedRequest(OwnerApi().deleteOwner(owner.id.get), "404") >> { _ => yieldValue(owner) } }
inline def ownerApiTest = step10

@main def hello: Unit =
  val output = debug(ownerApiTest)
  println(mgToGraphviz(toMg(ownerApiTest)))

  output match
    case (Result.Success(value), recs, logs) =>
      for log <- logs do
        println(log)
        println("----------------")
    case (Result.Failure(error), _, _) => print(error)
