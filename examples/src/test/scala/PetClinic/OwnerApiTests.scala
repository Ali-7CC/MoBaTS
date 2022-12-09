import org.scalatest.funsuite.AnyFunSuite
import DSL.*
import org.openapitools.client.api.*
import org.openapitools.client.model.*


class OwnerApiTests extends AnyFunSuite:

  test("'toMg' should handle simple end effects") {
    val result = toMg(end(1))
    val expected = Set((0,"END",1))
    assert(result == expected)
  }

    test("'toMg' should handle a sequence of end effects") {
    val result = toMg(end(1) >> { _ => end("2")})
    val expected = Set((0,"END",1), (1,"END",2))
    assert(result == expected)
  }


    test("'toMg' should handle simple request") {
    inline def req = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")))
    val result = toMg(req)
    val expected = Set((0,"request to OwnerApi.addOwner",1))
    assert(result == expected)
  }


    test("'toMg' should handle a simple sequence of requests") {
    inline def req1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201")
    inline def req2 = req1 >> { owner => request(OwnerApi().getOwner(owner.id.get), "200") }
    inline def req3 = req2 >> { owner => request(OwnerApi().deleteOwner(owner.id.get), "204") >> { _ => end(owner) }  }
    val result = toMg(req3)
    val expected = Set((0,"request to OwnerApi.addOwner. Expecting code 201",1), (1,"request to OwnerApi.getOwner. Expecting code 200",2), (2,"request to OwnerApi.deleteOwner. Expecting code 204",3), (3,"END",4))
    assert(result == expected)
  }

    test("'toMg' should handle an effect with recursion") {
    inline def e1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201") 
    >> {  _ => request(OwnerApi().addOwner(OwnerFields("John 2", "Doe 2", "Street 2 2", "Copenhagen 2", "60321321 2")), "201") } 
    inline def e2 = request(OwnerApi().listOwners(), "200") 
    inline def e3 = end("end")
    val result = toMg( rec("X", e1 >> { _ => e2 >> { _ => e3 >> { _ => loop("X")}}})  )

    val expected = Set((1,"request to OwnerApi.addOwner. Expecting code 201",2), (3,"END",4), (4,"",0), (2,"request to OwnerApi.listOwners. Expecting code 200",3), (0,"request to OwnerApi.addOwner. Expecting code 201",1))
    assert(result == expected)
  }

    test("'toMg' should handle an effect with choice") {
    inline def e1 = request(OwnerApi().addOwner(OwnerFields("John", "Doe", "Street 2", "Copenhagen", "60321321")), "201") 
    >> {  _ => request(OwnerApi().addOwner(OwnerFields("John 2", "Doe 2", "Street 2 2", "Copenhagen 2", "60321321 2")), "201") } 
    inline def e2 = request(OwnerApi().listOwners(), "200") 
    inline def e3 = end("end")
    val result = toMg( choose(e1, e2) >> { _ => e3} )

    val expected = Set((2,"",4), (4,"END",5), (3,"",4), (0,"request to OwnerApi.addOwner. Expecting code 201",1), (1,"request to OwnerApi.addOwner. Expecting code 201",2), (0,"request to OwnerApi.listOwners. Expecting code 200",3))
    assert(result == expected)
  }

end OwnerApiTests