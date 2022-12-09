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

import org.openapitools.client.model.Owner
import org.openapitools.client.model.OwnerFields
import org.openapitools.client.model.RestError
import org.openapitools.client.core.JsonSupport._
import sttp.client3._
import sttp.model.Method

object OwnerApi {

def apply(baseUrl: String = "http://localhost:9966/petclinic/api") = new OwnerApi(baseUrl)
}

class OwnerApi(baseUrl: String) {

  /**
   * Records the details of a new pet owner.
   * 
   * Expected answers:
   *   code 201 : Owner (The pet owner was sucessfully added.)
   *   code 400 : RestError (Bad request.)
   *   code 500 : RestError (Server error.)
   * 
   * @param ownerFields The pet owner
   */
  def addOwner(ownerFields: OwnerFields
): Request[Either[ResponseException[String, Exception], Owner], Any] =
    basicRequest
      .method(Method.POST, uri"$baseUrl/owners")
      .contentType("application/json")
      .body(ownerFields)
      .response(asJson[Owner])

  /**
   * Returns the owner or a 404 error.
   * 
   * Expected answers:
   *   code 204 :  (Update successful.)
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Owner  not found.)
   *   code 500 : RestError (Server error.)
   *   code 0 :  (default description)
   * 
   * @param ownerId The ID of the owner.
   */
  def deleteOwner(ownerId: Int
): Request[Either[Either[String, String], Unit], Any] =
    basicRequest
      .method(Method.DELETE, uri"$baseUrl/owners/${ownerId}")
      .contentType("application/json")
      .response(asEither(asString, ignore))

  /**
   * Returns the pet owner or a 404 error.
   * 
   * Expected answers:
   *   code 200 : Owner (Owner details found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Owner not found.)
   *   code 500 : RestError (Server error.)
   * 
   * @param ownerId The ID of the pet owner.
   */
  def getOwner(ownerId: Int
): Request[Either[ResponseException[String, Exception], Owner], Any] =
    basicRequest
      .method(Method.GET, uri"$baseUrl/owners/${ownerId}")
      .contentType("application/json")
      .response(asJson[Owner])

  /**
   * Returns an array of pet owners.
   * 
   * Expected answers:
   *   code 200 : Seq[Owner] (Owner details found and returned.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 304 :  (Not modified.)
   *              Headers :
   *                ETag - An ID for this version of the response.
   *   code 500 : RestError (Server error.)
   * 
   * @param lastName Last name.
   */
  def listOwners(lastName: Option[String] = None
): Request[Either[ResponseException[String, Exception], Seq[Owner]], Any] =
    basicRequest
      .method(Method.GET, uri"$baseUrl/owners?lastName=${ lastName }")
      .contentType("application/json")
      .response(asJson[Seq[Owner]])

  /**
   * Updates the pet owner record with the specified details.
   * 
   * Expected answers:
   *   code 204 :  (Update successful.)
   *   code 400 : RestError (Bad request.)
   *   code 404 : RestError (Owner not found.)
   *   code 500 : RestError (Server error.)
   *   code 0 :  (default description)
   * 
   * @param ownerId The ID of the pet owner.
   * @param ownerFields The pet owner details to use for the update.
   */
  def updateOwner(ownerId: Int, ownerFields: OwnerFields
): Request[Either[Either[String, String], Unit], Any] =
    basicRequest
      .method(Method.PUT, uri"$baseUrl/owners/${ownerId}")
      .contentType("application/json")
      .body(ownerFields)
      .response(asEither(asString, ignore))

}
