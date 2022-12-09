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
package org.openapitools.client.model


  /**
   * Owner
   * A pet owner.
   */
case class Owner(
  /* The first name of the pet owner. */
  firstName: String,
  /* The last name of the pet owner. */
  lastName: String,
  /* The postal address of the pet owner. */
  address: String,
  /* The city of the pet owner. */
  city: String,
  /* The telephone number of the pet owner. */
  telephone: String,
  /* The ID of the pet owner. */
  id: Option[Int] = None,
  /* The pets owned by this individual including any booked vet visits. */
  pets: Seq[Pet]
)

