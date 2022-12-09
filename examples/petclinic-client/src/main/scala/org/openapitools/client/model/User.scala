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
   * User
   * An user.
   */
case class User(
  /* The username */
  username: String,
  /* The password */
  password: Option[String] = None,
  /* Indicates if the user is enabled */
  enabled: Option[Boolean] = None,
  /* The roles of an user */
  roles: Option[Seq[Role]] = None
)

