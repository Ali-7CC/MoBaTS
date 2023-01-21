package PetClinic

import DSL.*
import org.openapitools.client.model.*
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import scala.util.Random
import org.openapitools.client.api.*
import org.openapitools.client.model.*

// Helper function to generate random data for UpdatedSettsModel.scala

private var idCounter = Map("PetType" -> 7, "Pet" -> 14, "Vet" -> 6, "Visit" -> 5, "Specialty" -> 4)

private def randomWord()    = Random.shuffle(('a' to 'z') ++ ('A' to 'Z')).take(7).mkString
private def randomPhone()   = Random.shuffle(('0' to '9')).take(10).mkString
private def randomId()      = Random.nextInt(Int.MaxValue)
private def randomAddress() = Random.alphanumeric.take(20).mkString
private def randomDate() =
  val year  = Random.nextInt(2022 - 2020) + 2020
  val month = Random.nextInt(12) + 1
  val day   = Random.nextInt(28) + 1
  LocalDate.of(year, month, day)

private def randomSentence() =
  val numWords = Random.nextInt(10) + 5
  (1 to numWords).map(_ => randomWord()).mkString(" ")

private def getId(apiName: String) =
  val id = idCounter.get(apiName).get
  idCounter = idCounter.updated(apiName, id + 1)
  id

private def getSomeSpecialties: Option[Seq[Specialty]] =
  val res = run(request(SpecialtyApi().listSpecialties(), "200") >> { specialties => yieldValue(specialties) })
  res match
    case Result.Success(v)     => Some(v)
    case Result.Failure(value) => None

private def getSomePetType: Option[PetType] =
  val res = run(request(PettypesApi().listPetTypes, "200") >> { pets => yieldValue(pets) })
  res match
    case Result.Success(v)     => Some(v(0))
    case Result.Failure(value) => None

private def getSomeOwnerId: Option[Int] =
  val res = run(request(OwnerApi().listOwners(), "200") >> { owners => yieldValue(owners) })
  res match
    case Result.Success(v)     => v(0).id
    case Result.Failure(value) => None

private def getSomeVisits: Option[Seq[Visit]] =
  val res = run(request(VisitApi().listVisits(), "200") >> { visits => yieldValue(visits) })
  res match
    case Result.Success(v)     => Some(v)
    case Result.Failure(value) => None

private def getSomePetId: Option[Int] =
  val res = run(request(PetApi().listPets(), "200") >> { pets => yieldValue(pets) })
  res match
    case Result.Success(v)     => Some(v(0).id)
    case Result.Failure(value) => None

private def generateRandomSpecialty()                                 = Specialty(getId("Specialty"), randomWord())
private def generateRandomOwnerFields()                               = OwnerFields(randomWord(), randomWord(), randomAddress(), randomWord(), randomPhone())
private def generateRandomPetType()                                   = PetType(randomWord(), getId("PetType"))
private def generateRandomVet()                                       = Vet(randomWord(), randomWord(), getSomeSpecialties.get, getId("Vet"))
private def generateRandomUser()                                      = User(randomWord())
private def generateRandomPet()                                       = Pet(randomWord(), randomDate(), getSomePetType.get, getId("Pet"), getSomeOwnerId, getSomeVisits.get)
private def generateRandomPetFields()                                 = PetFields(randomWord(), randomDate(), getSomePetType.get)
private def generateRandomVisit()                                     = Visit(Some(randomDate()), randomSentence(), randomId(), getSomePetId)
private def generateRandomVisitFields(date: Option[LocalDate] = None) = VisitFields(date, randomSentence())

private def runner[R](model: Model[R, Error], errors: Set[Error], start: Int, finish: Int): Set[Error] =
  finish match
    case f if f == start => errors
    case _ =>
      val res = run(model)
      res match
        case Result.Success(_)   => runner(model, errors, start + 1, finish)
        case Result.Failure(err) => runner(model, errors `+` (err), start + 1, finish)
