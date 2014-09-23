package nl.edejong.overpass

import org.json4s.JsonAST.{JField, JString, JObject}
import org.json4s.native.JsonParser
import org.json4s.native.Serialization._
import org.scalatest._

class OSMResponseModelSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  import OSMResponseModel._
  import org.json4s.native.Serialization.{read}
  implicit val formats = OSMResponseModel.osmResponseFormats

  "OSMResponseModel" should "be able to correctly parse a response without elements" in {

    val message = read[Message]("""{
                                  |  "version": 0.6,
                                  |  "generator": "Overpass API",
                                  |  "osm3s": {
                                  |    "timestamp_osm_base": "date",
                                  |    "copyright": ""
                                  |  },
                                  |  "elements": []
                                  |}""".stripMargin)
    message should be (Message(
      version = 0.6f,
      generator = "Overpass API",
      osm3s = OSM3S(timestamp_osm_base = "date", copyright = ""),
      elements = Seq()))
  }

  "OSMResponseModel" should "be able to correctly parse a sequence with one node  element" in {
    val input = """[{"type": "node", "id": 1, "lat": 2.0, "lon": -3.0 }]"""
    read[Seq[Element]](input) should be (List(Node(id = 1, lat = 2.0, lon = -3.0)))
  }

  "OSMResponseModel" should "be able to correctly parse a sequence with one way element" in {
    val input = """[{"type": "way", "id": 1, "nodes": [10, 11, 12], "tags": { "highway": "tertiary", "name": "Main Street" } }]"""
    read[Seq[Element]](input) should be (List(Way(
      id = 1,
      nodes = Seq(10,11,12),
      tags = Some(Map("highway" → "tertiary", "name" → "Main Street")))))
  }

  "OSMResponseModel" should "be able to correctly parse a sequence with one relation element" in {
    val input = """[{
                  |  "type": "relation",
                  |  "id": 1,
                  |  "members": [
                  |    {
                  |      "type": "relation",
                  |      "ref": 1745069,
                  |      "role": ""
                  |    },
                  |    {
                  |      "type": "relation",
                  |      "ref": 172789,
                  |      "role": ""
                  |    }
                  |  ],
                  |  "tags": {
                  |    "from": "Konrad-Adenauer-Platz",
                  |    "name": "VRS 636",
                  |    "network": "VRS",
                  |    "operator": "SWB",
                  |    "ref": "636",
                  |    "route": "bus",
                  |    "to": "Gielgen",
                  |    "type": "route_master",
                  |    "via": "Ramersdorf"
                  |  }
                  |}]""".stripMargin

    parseAndExtract[Seq[Element]](input) should be (
      Seq(Relation(1,
        List(
          RelationMember(1745069,""),
          RelationMember(172789,"")),
        Some(Map(
          "from" -> "Konrad-Adenauer-Platz",
          "name" -> "VRS 636",
          "network" -> "VRS",
          "operator" -> "SWB",
          "ref" -> "636",
          "route" -> "bus",
          "to" -> "Gielgen",
          "type" -> "route_master",
          "via" -> "Ramersdorf")))))
  }
}
