package PetClinic

import org.openapitools.client.model.*
import java.time.format.DateTimeFormatter
import java.time.LocalDate

val ownerFields1: OwnerFields = OwnerFields("John", "Doe", "Street 2", "Copenhagen", "12345678")
val ownerFields2: OwnerFields = OwnerFields("Jon", "Doe", "Street 2", "Copenhagen", "12345678")
val petType1                  = PetType("cat", 1)
val petType2                  = PetType("dog", 2)
val petFields1: PetFields     = PetFields("Kiwi", LocalDate.parse("2018-05-17"), petType1)
val petFields2                = PetFields.apply("Fulffy", LocalDate.parse("2020-08-01"), petType2)
val pet1                      = Pet("Kiwi", LocalDate.parse("2018-05-17"), petType1, 1, None, Seq.empty)
val pet2                      = Pet("Fluffy", LocalDate.parse("2020-08-01"), petType2, 2, None, Seq.empty)
val visitFields1              = VisitFields(None, "Kiwi is having a bad tummy")
val visit1                    = Visit(None, "Kiwi is having a bad tummy", 1, Some(1))
val visit2                    = Visit(None, "Kiwi is having a headache", 1, Some(1))
val specialty1                = Specialty(1, "Cats")
val specialty2                = Specialty(2, "Dogs")
val vet1                      = Vet("Dr.", "Jack", Seq(specialty1), 10)
val vet2                      = Vet("Dr.", "James", Seq(specialty2), 11)
val user1                     = User("Ali")
