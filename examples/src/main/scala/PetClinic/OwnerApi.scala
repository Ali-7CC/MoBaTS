package PetClinic

import DSL.*
import Graph.*
import org.openapitools.client.api._
import org.openapitools.client.model._

// inline def step1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
// inline def step2 = step1 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner => assertTrue(retrievedOwner, owner == retrievedOwner) } }
// inline def step3 = step2 >> { owner =>
//   request(OwnerApi().updateOwner(owner.id.get, OwnerFields("updatedName", "updatedLastName", "Updated Address", "Updated City", "12345678")), "204") >> { _ => yieldValue(owner) }
// }
// inline def step4 = step3 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") }
// inline def step5 = step4 >> { owner => request(OwnerApi().listOwners(), "200") >> { owners => assertTrue(owners, owners.contains(owner)) >> { _ => yieldValue(owner) } } }
// inline def step6 = step5 >> { owner => request(OwnerApi().listOwners(Some(owner.lastName)), "200") >> { owners => assertTrue(owners, owners.contains(owner)) >> { _ => yieldValue(owner) } } }
// inline def step7 = step6 >> { owner => request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue(owner) } }
// inline def step8 = step7 >> { owner => failedRequest(OwnerApi().getOwner(owner.id.get), "404") >> { _ => yieldValue(owner) } }
// inline def step9 = step8 >> { owner =>
//   failedRequest(OwnerApi().updateOwner(owner.id.get, OwnerFields("updatedName", "updatedLastName", "Updated Address", "Updated City", "12345678")), "404") >> { _ => yieldValue(owner) }
// }
// inline def step10       = step9 >> { owner => failedRequest(OwnerApi().deleteOwner(owner.id.get), "404") >> { _ => yieldValue(owner) } }
// inline def ownerApiTest = step10

@main def hello: Unit =
  inline def addOwner = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
  inline def deleteOwner(owner: Owner) = request(OwnerApi().deleteOwner(owner.id.get), "204") 
  inline def updateOwner(owner: Owner) = request(OwnerApi().updateOwner(owner.id.get, OwnerFields("updatedName", "updatedLastName", "Updated Address", "Updated City", "12345678")), "204") 


  inline def branch1(owner: Owner) = request(OwnerApi().deleteOwner(owner.id.get), "204")
  inline def branch2(owner: Owner) = rec("x", updateOwner(owner) >> {_ => loop("x")})
  inline def branch3 = request(OwnerApi().listOwners(),"200")

  inline def model = addOwner >> 
    { owner => choose(
        branch1(owner),
        branch2(owner),
        branch3
      ) >> 
        {_ => addOwner} 
    } 

  // println(mgToGraphviz(toMg(model)))
  inline def step1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
  inline def step2 = step1 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner => assertTrue(retrievedOwner, owner == retrievedOwner) } }
  inline def test = rec("x", step1 >> { owner => print("Heyyy"); if owner.id.get == 1 then loop("x") else yieldValue(())})

    println(mgToGraphviz(toMg(test)))

  


  // val output = debug(step2)
  // output match
  //   case (Result.Success(value), recs, logs) =>
  //     for log <- logs do
  //       println(log)
  //       println("----------------")
  //   case (Result.Failure(error), _, _) => print(error)
