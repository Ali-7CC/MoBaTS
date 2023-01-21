package PetClinic

import DSL.*

import org.openapitools.client.api.*
import org.openapitools.client.model.*


// The ownerApiModel
inline def ownerApiModel = 
  request(OwnerApi().addOwner(ownerFields1), "201") >> { owner => 
    rec { x => 
      choose(
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ => loop(x)},

        request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
              assertTrue(retrievedOwner, owner.id.get == retrievedOwner.id.get) >> { _ => loop(x)}},
            
        request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue((()))})}}


// Showcasing model composition
inline def addPetToOwner = 
  request(OwnerApi().addOwner(ownerFields1), "201") >> {owner => 
    request(PetApi().addPetToOwner(owner.id.get, petFields1), "201")}

inline def ownerApiModel2 =  ownerApiModel >> {_ => addPetToOwner}


// Showcasing model parameterization
inline def ownerApiTests(x: RecVar) = 
  request(OwnerApi().addOwner(ownerFields1), "201") >> {owner =>
    choose(
      request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ => loop(x)},

      request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
        assertTrue(retrievedOwner, owner.id.get == retrievedOwner.id.get) >> { _ => loop(x)}},

      request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue((()))}
    )}


inline def petClinicTests = rec {x => 
  choose(
    ownerApiTests(x),
    yieldValue(())
  )}

  
// Non-regular model. Cannot be graphed!
inline def addOwner(ownerFields: OwnerFields): Model[Owner, Error] =  request(OwnerApi().addOwner(ownerFields), "201")
inline def deleteOwner(ownerId: Int):  Model[Unit, Error]  = request(OwnerApi().deleteOwner(ownerId), "204")

def nonRegularModel(): Model[Unit, Error] = rec {x => 
  choose(
    addOwner(ownerFields1) >> {owner => nonRegularModel() >> {_ => deleteOwner(owner.id.get)}},
    yieldValue(()))}


// Additional models that showcase the DSL features
inline def modelA = 
  request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
    rec { x =>
      choose(
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ =>
          request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
            assertTrue(retrievedOwner, retrievedOwner.firstName != owner.firstName) >> { _ => loop(x)}}},

        request(OwnerApi().listOwners(Some(owner.lastName)), "200") >> { owners =>
          assertTrue(owners, owners.exists(o => o.lastName == "Doe")) >> { _ => yieldValue(())}},

        request(OwnerApi().getOwner(owner.id.get), "200") >> { rOwner =>
          if rOwner.firstName == "John" then
            loop(x) 
          else 
            request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue(()) }})}}

// Has a failing branch
inline def modelB = request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
  rec { x =>
    request(PetApi().addPetToOwner(owner.id.get, petFields1), "201") >> { pet =>
      request(PetApi().listPets(), "200") >> { _ =>
        choose(
          request(PetApi().updatePet(pet.id, pet2), "204") >> { _ =>
            request(PetApi().getPet(pet.id), "200")
          },

          request(PetApi().updateOwnersPet(owner.id.get, pet.id, petFields2), "204") >> { _ =>
            request(PetApi().getPet(pet.id), "200")
          }
        ) >> { updatedPet =>
          assertTrue(updatedPet, updatedPet.name == "UpdatedPetName") >> { _ =>
            choose(loop(x), request(PetApi().deletePet(pet.id), "204") >> { _ => yieldValue(()) })}}}}}}