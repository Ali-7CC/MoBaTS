/**
 * Spring PetClinic
 * Spring PetClinic Sample Application.
 *
 * The version of the OpenAPI document: 1.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */
package org.openapitools.client.api

import org.openapitools.client.model.RestError
import org.openapitools.client.model.Specialty
import org.openapitools.client.core.JsonSupport._
import sttp.client3._
import sttp.model.Method

object SpecialtyApi {

def apply(baseUrl: String = "http://localhost:9966/petclinic/api") = new SpecialtyApi(baseUrl)
}

class SpecialtyApi(baseUrl: String) {

  /**
   * Creates a specialty .
   * 
   * Expected answers:
   *   code 200 : Specialty (Specialty created successfully.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Specialty not found.)
   *   code 500 : RestError (Server error.)
   * 
   * @param specialty The specialty
   */
  def addSpecialty(specialty: Specialty
): Request[Either[ResponseException[String, Exception], Specialty], Any] =
    basicRequest
      .method(Method.POST, uri"$baseUrl/specialties")
      .contentType("application/json")
      .body(specialty)
      .response(asJson[Specialty])

  /**
   * Returns the specialty or a 404 error.
   * 
   * Expected answers:
   *   code 200 : Specialty (Specialty details found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Specialty not found.)
   *   code 500 : RestError (Server error.)
   * 
   * @param specialtyId The ID of the specialty.
   */
  def deleteSpecialty(specialtyId: Int
): Request[Either[ResponseException[String, Exception], Specialty], Any] =
    basicRequest
      .method(Method.DELETE, uri"$baseUrl/specialties/${specialtyId}")
      .contentType("application/json")
      .response(asJson[Specialty])

  /**
   * Returns the specialty or a 404 error.
   * 
   * Expected answers:
   *   code 200 : Specialty (Specialty details found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Specialty not found.)
   *   code 500 : RestError (Server error.)
   * 
   * @param specialtyId The ID of the pet.
   */
  def getSpecialty(specialtyId: Int
): Request[Either[ResponseException[String, Exception], Specialty], Any] =
    basicRequest
      .method(Method.GET, uri"$baseUrl/specialties/${specialtyId}")
      .contentType("application/json")
      .response(asJson[Specialty])

  /**
   * Returns an array of specialty .
   * 
   * Expected answers:
   *   code 200 : Seq[Specialty] (Specialties found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 500 : RestError (Server error.)
   */
  def listSpecialties(
): Request[Either[ResponseException[String, Exception], Seq[Specialty]], Any] =
    basicRequest
      .method(Method.GET, uri"$baseUrl/specialties")
      .contentType("application/json")
      .response(asJson[Seq[Specialty]])

  /**
   * Returns the specialty or a 404 error.
   * 
   * Expected answers:
   *   code 200 : Specialty (Specialty details found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Specialty not found.)
   *   code 500 : RestError (Server error.)
   * 
   * @param specialtyId The ID of the specialty.
   * @param specialty The pet
   */
  def updateSpecialty(specialtyId: Int, specialty: Specialty
): Request[Either[ResponseException[String, Exception], Specialty], Any] =
    basicRequest
      .method(Method.PUT, uri"$baseUrl/specialties/${specialtyId}")
      .contentType("application/json")
      .body(specialty)
      .response(asJson[Specialty])

}
