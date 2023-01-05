package PetClinic

import DSL.*

import org.openapitools.client.api.*
import org.openapitools.client.model.*
import java.time.LocalDate


inline def modelA =
  request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
    rec { x =>
      choose(
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ =>
          request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
            assertTrue(retrievedOwner, retrievedOwner.firstName != owner.firstName) >> { _ =>
              loop(x)}}},


        request(OwnerApi().listOwners(Some(owner.lastName)), "200") >> { owners =>
          assertTrue(owners, owners.exists(o => o.lastName == "Doe")) >> { _ =>
            endLoop()}},

        request(OwnerApi().getOwner(owner.id.get), "200") >> { rOwner => 
          if rOwner.firstName == "John" then 
            loop(x) else 
            request(OwnerApi().deleteOwner(owner.id.get), "204") >> {_ => endLoop()}})}}



inline def modelB = request(OwnerApi().addOwner(ownerFields1), "201") >> { owner =>
  rec { x =>
    request(PetApi().addPetToOwner(owner.id.get, petFields1), "201") >> { pet =>
      request(PetApi().listPets(), "200") >> { _ =>
        choose(
          request(PetApi().updatePet(pet.id, pet2), "204") >> {_ => 
            request(PetApi().getPet(pet.id), "200")},


          request(PetApi().updateOwnersPet(owner.id.get, pet.id, petFields2), "204") >> { _ =>
            request(PetApi().getPet(pet.id), "200")}) >> { updatedPet =>
        assertTrue(updatedPet, updatedPet.name == "Fluffy") >> { _ =>
          choose(
            loop(x),
            request(PetApi().deletePet(pet.id), "204") >> { _ => endLoop() })}}}}}}
