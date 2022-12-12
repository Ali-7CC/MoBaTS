import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import DSL.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*

class OwnerApiTests extends AnyFunSuite with BeforeAndAfter:

  inline def addOwner       = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
  inline def listOwners     = request(OwnerApi().listOwners(), "200")
  inline def addThenGet     = addOwner >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") }
  inline def addDeleteYield = addOwner >> { owner => request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue(owner) } }

  test("'toMg' should a simple request") {
    val result   = toMg(addOwner)
    val expected = Set((0, "request to OwnerApi.addOwner. Expecting code 201", 1))
    assert(result == expected)
  }

  test("'toMg' should handle a sequence of requests") {
    val result   = toMg(addThenGet)
    val expected = Set((0, "request to OwnerApi.addOwner. Expecting code 201", 1), (1, "request to OwnerApi.getOwner. Expecting code 200", 2))
    assert(result == expected)
  }

  test("'toMg' should handle recursion") {
    val result = toMg(rec("X", addThenGet >> { owner => request(OwnerApi().deleteOwner(owner.id.get)) } >> { _ => loop("X") }))
    val expected = Set(
      (0, "request to OwnerApi.addOwner. Expecting code 201", 1),
      (1, "request to OwnerApi.getOwner. Expecting code 200", 2),
      (2, "request to OwnerApi.deleteOwner", 3),
      (3, "", 0)
    )
    assert(result == expected)
  }
  test("'toMg' should handle choice (branching)") {
    val result = toMg(choose(addThenGet, listOwners))
    val expected = Set(
      (0, "request to OwnerApi.listOwners. Expecting code 200", 3),
      (0, "request to OwnerApi.addOwner. Expecting code 201", 1),
      (1, "request to OwnerApi.getOwner. Expecting code 200", 2)
    )
    assert(result == expected)
  }

  test("'toMg' should handle choice succeeded by sequence ") {
    val result = toMg(choose(addThenGet, addDeleteYield) >> { _ => listOwners })
    val expected = Set(
      (4, "", 5),
      (2, "", 5),
      (0, "request to OwnerApi.addOwner. Expecting code 201", 1),
      (0, "request to OwnerApi.addOwner. Expecting code 201", 3),
      (1, "request to OwnerApi.deleteOwner. Expecting code 204", 2),
      (3, "request to OwnerApi.getOwner. Expecting code 200", 4),
      (5, "request to OwnerApi.listOwners. Expecting code 200", 6)
    )
    assert(result == expected)
  }

end OwnerApiTests
