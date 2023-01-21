package PetClinic

import org.openapitools.client.model.*
import java.time.format.DateTimeFormatter
import java.time.LocalDate

// Mock data taken from SeTTS to replicate its PetClinic API test
val df                        = DateTimeFormatter.ofPattern("yyyy/MM/dd")
val ownerFields1: OwnerFields = OwnerFields("John", "Doe", "Street 2", "Copenhagen", "12345678")
val ownerFields2: OwnerFields = OwnerFields("Jon", "Doe", "Street 2", "Copenhagen", "12345678")
val petType1                  = PetType("testPetType", 7)
val petType2                  = PetType("updatedTestPetType", 7)
val petFields1                = PetFields("petName", LocalDate.parse("2020/12/10", df), PetType("cat", 1))
val petFields2                = PetFields.apply("Fulffy", LocalDate.parse("2020-08-01"), petType2)
val pet1                      = Pet("petName", LocalDate.parse("2020/12/10", df), PetType("cat", 1), 14, Some(10), Seq())
val pet2                      = Pet("UpdatedPetName", LocalDate.parse("2022/12/10", df), PetType("cat", 1), 14, Some(10), Seq())
val visitFields1              = VisitFields(Some(LocalDate.parse("2022/12/10", df)), "test description")
val visit1                    = Visit(Some(LocalDate.parse("2022/12/10", df)), "Test Description", 1, Some(14))
val visit2                    = Visit(Some(LocalDate.parse("2019/10/01", df)), "Updated Test Description", 1, Some(14))
val specialty1                = Specialty(4, "testSpecialty")
val specialty2                = Specialty(4, "updatedTestSpeciality")
val vet1                      = Vet("VetName", "VetLastName", Seq(), 1)
val vet2                      = Vet("UpdatedVetName", "UpdatedVetLastName", Seq(Specialty(2, "surgery")), 1)
val user1                     = User("Ali")
