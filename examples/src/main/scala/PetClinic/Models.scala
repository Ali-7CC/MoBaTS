package PetClinic

import DSL.*

import org.openapitools.client.api.*
import org.openapitools.client.model.*
import java.time.format.DateTimeFormatter

import java.time.LocalDate
val df = DateTimeFormatter.ofPattern("yyyy/MM/dd")


val ownerFields1: OwnerFields = OwnerFields("John", "Doe", "Street 2", "Copenhagen", "12345678")
val ownerFields2: OwnerFields = OwnerFields("Jon", "Doe", "Street 2", "Copenhagen", "12345678")
val petType1                  = PetType("cat", 1)
val petType2                  = PetType("dog", 2)
val petFields1: PetFields                = PetFields("Kiwi", LocalDate.parse("2018-05-17"), petType1)
val petFields2                = PetFields.apply("Fulffy", LocalDate.parse("2020/08/01", df), petType2)

inline def modelA =
  request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
    rec { x =>
      choose(
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ =>
          request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
            assertTrue(retrievedOwner, retrievedOwner.firstName != owner.firstName) >> { _ =>
              loop(x)
            }
          }
        },
        request(OwnerApi().listOwners(Some(owner.lastName)), "200") >> { owners =>
          assertTrue(owners, owners.exists(o => o.lastName == "Doe")) >> { _ =>
            endLoop()
          }
        },

        request(OwnerApi().getOwner(owner.id.get), "200") >> { rOwner => 
          if rOwner.firstName == "John" then 
            loop(x) else 
            request(OwnerApi().deleteOwner(owner.id.get), "204") >> {_ => endLoop()} }
      ) 
    }
  }

inline def modelB = request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
  rec { x =>
    request(PetApi().addPetToOwner(owner.id.get, petFields1), "201") >> { pet =>
      request(PetApi().listPets(), "200") >> { _ =>
        choose(
          request(PetApi().updatePet(pet.id, Pet("Fluffy", LocalDate.parse("2020-08-01"), petType2, pet.id, None, Seq.empty)), "201") >> {_ => 
            request(PetApi().getPet(pet.id), "200")},
          request(PetApi().updateOwnersPet(owner.id.get, pet.id, petFields2), "204") >> { _ =>
            request(PetApi().getPet(pet.id), "200")
          }
        ) >> { updatedPet =>
          assertTrue(updatedPet, updatedPet.name == "Fluffy") >> { _ =>
            choose(
              loop(x),
              request(PetApi().deletePet(pet.id), "204") >> { _ => endLoop() }
            )
          }
        }
      }

    }
  }
}


inline def testModel = request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
    request(PetApi().addPetToOwner(owner.id.get, petFields1), "201") >> { pet => 
        request(OwnerApi().getOwner(owner.id.get), "200") >> {updatedOwner => 
            request(PetApi().getOwnersPet(updatedOwner.id.get, updatedOwner.pets(0).id))}
        }

}
