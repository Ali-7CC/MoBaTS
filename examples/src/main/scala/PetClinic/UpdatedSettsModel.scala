package PetClinic

import DSL.*

import org.openapitools.client.api.*
import org.openapitools.client.model.*

// SeTTS PetClinic model written in MoBaTS, however with random generators
inline def updatedPetClinicTest = rec { x =>
  choose(
    updatedOwnerApiTest(x),
    updatedPetApiTest(x),
    updatedVetApiTest(x),
    updatedPetTypeApiTest(x),
    updatedSpecialtyApiTest(x),
    updatedUserApiTest(x),
    updatedFailingApiTest
  )
}


inline def updatedOwnerApiTest(x: RecVar) =
  request(OwnerApi().addOwner(generateRandomOwnerFields()), "201") >> { owner =>
    request(OwnerApi().getOwner(owner.id.get), "200") >> { retrievedOwner =>
      assertTrue(retrievedOwner, retrievedOwner.id.get == owner.id.get) >> { _ =>
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> { _ =>
          request(OwnerApi().getOwner(owner.id.get), "200") >> { updatedOwner =>
            request(OwnerApi().listOwners(), "200") >> { owners =>
              assertTrue(updatedOwner, owners.contains(updatedOwner)) >> { _ =>
                request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ =>
                  failedRequest(OwnerApi().getOwner(owner.id.get), "404") >> { _ =>
                    failedRequest(OwnerApi().updateOwner(owner.id.get, ownerFields2), "404") >> { _ =>
                      failedRequest(OwnerApi().deleteOwner(owner.id.get), "404") >> { _ => loop(x) }}}}}}}}}}}


inline def updatedPetApiTest(x: RecVar) =
  request(PetApi().listPets(), "200") >> { pets =>
    choose(
      request(PetApi().addPet(generateRandomPet()), "200") >> { _ => loop(x) },

      request(PetApi().getPet(getSomePetId.get), "200") >> { pet =>
        request(OwnerApi().addOwner(generateRandomOwnerFields()), "201") >> { owner =>
          request(PetApi().addPetToOwner(owner.id.get, generateRandomPetFields()), "201") >> { createdPet =>
            assertTrue(pets, !pets.isEmpty) >> { _ =>
              choose(
                request(PetApi().getOwnersPet(owner.id.get, createdPet.id)) >> { ownersPet =>
                  assertTrue(ownersPet, ownersPet == createdPet) >> { _ => loop(x) }},

                request(PetApi().updatePet(createdPet.id, pet2), "204") >> { _ =>
                  request(PetApi().getPet(createdPet.id), "200") >> { updatedPet =>
                    assertTrue(updatedPet, updatedPet != createdPet) >> { _ =>
                      request(PetApi().deletePet(createdPet.id), "204") >> { _ =>
                        failedRequest(PetApi().getPet(createdPet.id), "404") >> { _ =>
                          failedRequest(PetApi().updatePet(createdPet.id, pet2), "404") >> { _ =>
                            failedRequest(PetApi().deletePet(createdPet.id), "404") >> { _ =>
                              request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => loop(x) }}}}}}}},

                request(VisitApi().addVisitToOwner(owner.id.get, pet.id, generateRandomVisitFields()), "201") >> { visit =>
                  request(VisitApi().getVisit(visit.id), "200") >> { retrievedVisit =>
                    assertTrue(retrievedVisit, visit == retrievedVisit) >> { _ =>
                      request(VisitApi().updateVisit(visit.id, visit2), "204") >> { _ =>
                        request(VisitApi().listVisits(), "200") >> { visits =>
                          request(VisitApi().deleteVisit(visit.id), "204") >> { _ =>
                            failedRequest(VisitApi().getVisit(visit.id), "404") >> { _ =>
                              failedRequest(VisitApi().updateVisit(visit.id, visit2), "404") >> { _ =>
                                failedRequest(VisitApi().deleteVisit(visit.id), "404") >> { _ =>
                                  request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => loop(x) }}}}}}}}}})}}}},

      request(VisitApi().addVisit(generateRandomVisit()), "201") >> { visit =>
        request(VisitApi().getVisit(visit.id), "200") >> { retrievedVisit =>
          assertTrue(retrievedVisit, retrievedVisit == visit) >> { _ => loop(x) }}})}



inline def updatedVetApiTest(x: RecVar) = 
    request(VetApi().addVet(generateRandomVet()), "201") >> { vet =>
        choose(
            request(VetApi().listVets(), "200") >> { vets =>
                assertTrue(vet, vets.contains(vet)) >> { _ => loop(x) }},

            request(VetApi().getVet(vet.id), "200") >> { vet =>
              request(VetApi().updateVet(vet.id, vet2), "204") >> { updatedVet =>
                request(VetApi().listVets(), "200") >> { vets =>
                  assertTrue(updatedVet, vets.contains(updatedVet)) >> { _ =>
                    request(VetsApi().deleteVet(vet.id), "204") >> { _ =>
                      failedRequest(VetApi().getVet(vet.id), "404") >> { _ =>
                        failedRequest(VetApi().updateVet(vet.id, vet2), "404") >> { _ =>
                          failedRequest(VetsApi().deleteVet(vet.id), "404") >> { _ => loop(x) }}}}}}}})}


inline def updatedPetTypeApiTest(x: RecVar) =
  request(PettypesApi().addPetType(generateRandomPetType()), "201") >> { petType =>
    request(PettypesApi().listPetTypes(), "200") >> { petTypes =>
      request(PettypesApi().getPetType(petType.id), "200") >> { retrievedPetType =>
        assertTrue(retrievedPetType, petType == retrievedPetType) >> { _ =>
          request(PettypesApi().updatePetType(petType.id, petType2), "204") >> { _ =>
            request(PettypesApi().deletePetType(petType.id), "204") >> { _ =>
              failedRequest(PettypesApi().getPetType(petType.id), "404") >> { _ =>
                failedRequest(PettypesApi().updatePetType(petType.id, petType2), "404") >> { _ =>
                  failedRequest(PettypesApi().deletePetType(petType.id), "404") >> { _ => loop(x) }}}}}}}}}


inline def updatedSpecialtyApiTest(x: RecVar) =
  request(SpecialtyApi().addSpecialty(generateRandomSpecialty()), "201") >> { specialty =>
    request(SpecialtyApi().listSpecialties(), "200") >> { specialties =>
      assertTrue(specialty, specialties.contains(specialty)) >> {_ => 
        request(SpecialtyApi().updateSpecialty(specialty.id, specialty2), "204") >> { _ =>
          request(SpecialtyApi().getSpecialty(specialty.id), "200") >> { retrievedSpecialty =>
            request(SpecialtyApi().deleteSpecialty(specialty.id), "204") >> { _ =>
              failedRequest(SpecialtyApi().getSpecialty(specialty.id), "404") >> { _ =>
                failedRequest(SpecialtyApi().updateSpecialty(specialty.id, specialty2), "404") >> { _ =>
                  failedRequest(SpecialtyApi().deleteSpecialty(specialty.id), "404") >> { _ => loop(x) }}}}}}}}}

inline def updatedFailingApiTest = failedRequest(FailingApi().failingRequest(), "404") >> { _ => yieldValue(()) }

inline def updatedUserApiTest(x: RecVar) = request(UserApi().addUser(user1), "201") >> { _ => loop(x) }
