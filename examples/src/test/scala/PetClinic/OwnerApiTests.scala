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
    val expected = Set((0, "!OwnerApi.addOwner", 1), (1, "?201", 2))
    assert(result == expected)
  }

  test("'toMg' should handle a sequence of requests") {
    val result   = toMg(addThenGet)
    val expected = Set((0, "!OwnerApi.addOwner", 1), (1, "?201", 2), (2, "!OwnerApi.getOwner", 3), (3, "?200", 4))
    assert(result == expected)
  }

  test("'toMg' should handle recursion") {
    val result   = toMg(rec { x => addThenGet >> { owner => request(OwnerApi().deleteOwner(owner.id.get)) } >> { _ => loop(x) } })
    val expected = Set((2, "?201", 3), (0, "rec(x)", 1), (4, "?200", 5), (5, "!OwnerApi.deleteOwner", 6), (3, "!OwnerApi.getOwner", 4), (1, "!OwnerApi.addOwner", 2), (7, "loop(x)", 1))
    assert(result == expected)
  }
  test("'toMg' should handle choice (branching)") {
    val result   = toMg(choose(addThenGet, listOwners))
    val expected = Set((5, "?200", 6), (4, "!OwnerApi.getOwner", 5), (0, "!OwnerApi.listOwners", 1), (3, "?201", 4), (0, "!OwnerApi.addOwner", 3), (1, "?200", 2))
    assert(result == expected)
  }

  test("'toMg' should handle choice succeeded by sequence ") {
    val result = toMg(choose(addThenGet, addDeleteYield) >> { _ => listOwners })
    val expected = Set(
      (6, "!OwnerApi.getOwner", 7),
      (0, "!OwnerApi.addOwner", 1),
      (10, "?200", 11),
      (0, "!OwnerApi.addOwner", 5),
      (2, "!OwnerApi.deleteOwner", 3),
      (7, "?200", 8),
      (9, "!OwnerApi.listOwners", 10),
      (3, "?204", 4),
      (4, "", 9),
      (5, "?201", 6),
      (8, "", 9),
      (1, "?201", 2)
    )
    assert(result == expected)
  }

end OwnerApiTests
