import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import DSL.*
import Graph.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*

class PetClinicTests extends AnyFunSuite with BeforeAndAfter:

  inline def addOwner       = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
  inline def listOwners     = request(OwnerApi().listOwners(), "200")
  inline def addThenGet     = addOwner >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") }
  inline def addDeleteYield = addOwner >> { owner => request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => yieldValue(owner) } }

  test("'modelToGraph' should a simple request") {
    val result   = modelToGraph(addOwner)
    val expected = Set((0, "!OwnerApi.addOwner", 1), (1, "?201", 2))
    assert(result == expected)
  }

  test("'modelToGraph' should handle a sequence of requests") {
    val result   = modelToGraph(addThenGet)
    val expected = Set((0, "!OwnerApi.addOwner", 1), (1, "?201", 2), (2, "!OwnerApi.getOwner", 3), (3, "?200", 4))
    assert(result == expected)
  }

  test("'modelToGraph' should handle recursion") {
    val result   = modelToGraph(rec { x => addThenGet >> { owner => request(OwnerApi().deleteOwner(owner.id.get)) } >> { _ => loop(x) } })
    val expected =  Set((6, "loop(x)", 1), (2, "?201", 3), (0, "rec(x)", 1), (4, "?200", 5), (1, "!OwnerApi.addOwner", 2), (5, "!OwnerApi.deleteOwner", 6), (3, "!OwnerApi.getOwner", 4))
    assert(result == expected)
  }
  test("'modelToGraph' should handle choice (branching)") {
    val result   = modelToGraph(choose(addThenGet, addOwner))
    val expected =  Set((0, "!OwnerApi.addOwner", 1), (5, "?200", 6), (4, "!OwnerApi.getOwner", 5), (1, "?201", 2), (3, "?201", 4), (0, "!OwnerApi.addOwner", 3))
    assert(result == expected)
  }

  test("'modelToGraph' should handle choice succeeded by sequence ") {
    val result = modelToGraph(choose(addThenGet, addDeleteYield) >> { _ => listOwners })
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

end PetClinicTests
