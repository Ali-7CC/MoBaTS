package PetClinic

import DSL.*

import org.openapitools.client.api.*
import org.openapitools.client.model.*


inline def settsTest = rec {x => 
  choose(
    ownerApiTest(x),
    petApiTest(x),
    vetApiTest(x),
    petTypeApiTest(x),
    specialtyApiTest(x),
    userApiTest(x),
    failingApiTest
  )
  }

inline def ownerApiTest(x: RecVar)  = 
  request(OwnerApi().addOwner(ownerFields1), "201") >> {owner => 
    request(OwnerApi().getOwner(owner.id.get), "200") >> {retrievedOwner => 
      assertTrue(retrievedOwner, retrievedOwner.id.get == owner.id.get) >> {_ =>
        request(OwnerApi().updateOwner(owner.id.get, ownerFields2), "204") >> {_ => 
          request(OwnerApi().getOwner(owner.id.get), "200") >> {updatedOwner => 
            request(OwnerApi().listOwners(), "200") >> {owners => 
              assertTrue(updatedOwner, owners.contains(updatedOwner)) >> {_ => 
                request(OwnerApi().deleteOwner(owner.id.get), "204") >> {_ => 
                  failedRequest(OwnerApi().getOwner(owner.id.get), "404") >> {_ =>
                    failedRequest(OwnerApi().updateOwner(owner.id.get, ownerFields1), "404") >> {_ => 
                      failedRequest(OwnerApi().deleteOwner(owner.id.get), "404") >> {_ => loop(x)}}}}}}}}}}}



inline def petApiTest(x: RecVar) = 
  request(PetApi().listPets(), "200") >> {pets => 
    choose(
      request(PetApi().addPet(pet1), "200") >> {_ => loop(x)},
      
      request(PetApi().getPet(1), "200") >> {pet => 
         request(OwnerApi().addOwner(ownerFields1), "201") >> {owner => 
          request(PetApi().addPetToOwner(owner.id.get, petFields1), "201") >> {createdPet => 
            assertTrue(pets, pets.isEmpty) >> {_ => 
              choose(
                request(PetApi().getOwnersPet(owner.id.get, createdPet.id)) >> {ownersPet => 
                  assertTrue(ownersPet, ownersPet == createdPet) >> {_ => loop(x)}},

                request(PetApi().updatePet(createdPet.id, pet2), "204") >> {_ => 
                  request(PetApi().getPet(createdPet.id), "200") >> {updatedPet => 
                    assertTrue(updatedPet, updatedPet == createdPet) >> {_  => 
                      request(PetApi().deletePet(createdPet.id), "204") >> {_ => 
                        failedRequest(PetApi().getPet(createdPet.id), "404") >> {_ => 
                          failedRequest(PetApi().updatePet(createdPet.id, pet1), "404") >> {_ => 
                            failedRequest(PetApi().deletePet(createdPet.id), "404") >> {_ => 
                              request(OwnerApi().deleteOwner(owner.id.get), "204") >> {_ => loop(x)}}}}}}}},


                  request(VisitApi().addVisitToOwner(owner.id.get, pet.id, visitFields1), "201") >> {visit => 
                    request(VisitApi().getVisit(visit.id), "200") >> {retrievedVisit => 
                      assertTrue(retrievedVisit, visit == retrievedVisit) >> {_ => 
                        request(VisitApi().updateVisit(visit.id, visit2), "204") >> {_ => 
                          request(VisitApi().listVisits(), "200") >> {visits => 
                            request(VisitApi().deleteVisit(visit.id), "204") >> {_ => 
                              failedRequest(VisitApi().getVisit(visit.id), "404") >> {_ => 
                                failedRequest(VisitApi().updateVisit(visit.id, visit2), "404") >> {_ => 
                                  failedRequest(VisitApi().deleteVisit(visit.id), "404") >> {_ =>
                                    request(OwnerApi().deleteOwner(owner.id.get), "204") >> {_ => loop(x)}}}}}}}}}})}}}},


    request(VisitApi().addVisit(visit1), "201") >> {visit => 
      request(VisitApi().getVisit(visit.id), "200") >> {retrievedVisit => 
        assertTrue(retrievedVisit, retrievedVisit == visit) >> {_ => loop(x)}}})}



inline def vetApiTest(x: RecVar) = request(VetApi().addVet(vet1), "201") >> {vet => 
  choose(
    request(VetApi().listVets(), "200") >> {vets => 
      assertTrue(vet, vets.contains(vet)) >> {_ => loop(x)}},

    request(VetApi().updateVet(vet.id, vet2), "204") >> {_ => 
      request(VetApi().getVet(vet.id), "200") >> {updatedVet => 
        request(VetApi().listVets(), "200") >> {vets => 
          assertTrue(updatedVet, vets.contains(updatedVet)) >> {_ => 
            request(VetsApi().deleteVet(vet.id), "204") >> {_ => 
              failedRequest(VetApi().getVet(vet.id), "404") >> {_ =>
                failedRequest(VetApi().updateVet(vet.id, vet2), "404") >> {_ => 
                  failedRequest(VetsApi().deleteVet(vet.id), "404") >> {_ => loop(x)}}}}}}}})}

    
inline def petTypeApiTest(x: RecVar) = 
  request(PettypesApi().addPetType(petType1), "201") >> {petType => 
    request(PettypesApi().listPetTypes(), "200") >> {petTypes => 
      request(PettypesApi().getPetType(petType.id), "200") >> {retrievedPetType => 
        assertTrue(retrievedPetType, petType == retrievedPetType) >> {_ => 
          request(PettypesApi().updatePetType(petType.id, petType2), "204") >> {_ => 
            request(PettypesApi().deletePetType(petType.id), "204") >> {_ => 
              failedRequest(PettypesApi().getPetType(petType.id), "404") >> {_ => 
                failedRequest(PettypesApi().updatePetType(petType.id, petType2), "404") >> {_ => 
                  failedRequest(PettypesApi().deletePetType(petType.id), "404") >> {_ => loop(x)}}}}}}}}}


inline def specialtyApiTest(x: RecVar) = 
  request(SpecialtyApi().addSpecialty(specialty1), "201") >> {specialty => 
    request(SpecialtyApi().listSpecialties(), "200") >> {specialties => 
      request(SpecialtyApi().updateSpecialty(specialty.id, specialty2), "204") >> {_ => 
        request(SpecialtyApi().getSpecialty(specialty.id), "200") >> {retrievedSpecialty => 
          request(SpecialtyApi().deleteSpecialty(specialty.id), "204") >> {_ => 
            failedRequest(SpecialtyApi().getSpecialty(specialty.id), "404") >> {_ => 
              failedRequest(SpecialtyApi().updateSpecialty(specialty.id, specialty2), "404") >> {_ => 
                failedRequest(SpecialtyApi().deleteSpecialty(specialty.id), "404") >> {_ => loop(x)}}}}}}}}

                    

inline def failingApiTest = failedRequest(FailingApi().failingRequest(), "404") >> {_ => endLoop()}

inline def userApiTest(x: RecVar) = request(UserApi().addUser(user1), "201") >> {_ => loop(x)}



