package nl.edejong.overpass

import java.util.Date

import org.json4s.JsonAST.JString
import org.json4s.native.{JsonParser, Serialization}

import scalaz.geo.Coord

object OSMResponseModel {
  case class OSM3S(
    timestamp_osm_base: String,
    copyright: String)

  sealed trait Element { self ⇒
    def id: Long
    def parentRelationships(implicit rs: ResultSet): Iterable[Relation] =
      for (rel ← rs.rels if rel.members.exists(_.ref == id)) yield rel
  }

  object RelationMember {
    def unapply(relationMember: RelationMember)(implicit rs: ResultSet): Option[Element] = rs.elements.get(relationMember.ref)
  }

  trait RelationshipMember {
     def role: String
     def ref: Long

    def element(implicit rs: ResultSet) = rs.elements get ref toLeft ref
  }

  case class RelationMember(ref: Long, role: String) extends RelationshipMember
  case class WayMember(ref: Long, role: String) extends RelationshipMember
  case class NodeMember(ref: Long, role: String) extends RelationshipMember

  case class Node(
                   id: Long,
                   lat: Double,
                   lon: Double,
                   tags: Option[Map[String, String]] = None) extends Element {

    def parentWays(implicit rs: ResultSet): Iterable[Way] =
      for (way ← rs.ways if way.nodes contains id) yield way
  }

  case class Way(
                  id: Long,
                  nodes: Seq[Long],
                  tags: Option[Map[String, String]]) extends Element {

    def findNodes(implicit rs: ResultSet): Seq[Either[Node, Long]] =
      for (nodeId ← nodes) yield
        rs.elements.get(nodeId) map (_.asInstanceOf[Node]) toLeft nodeId
  }

  case class Relation(
                  id: Long,
                  members: Seq[RelationshipMember],
                  tags: Option[Map[String, String]]) extends Element {
    def wayMembers(implicit rs: ResultSet): Seq[Either[Way, Long]] =
      members collect { case w: WayMember ⇒
        rs.elements.get(w.ref) map (_.asInstanceOf[Way]) toLeft w.ref }
  }

  case class ResultSet(elements: Map[Long, Element] = Map()) {
    def augment(element: Element): ResultSet =
      ResultSet(elements = (elements + (element.id → element)))

    def nodes: Iterable[Node] = elements.values.collect { case n: Node ⇒ n }
    def ways: Iterable[Way] = elements.values.collect { case w: Way ⇒ w }
    def rels: Iterable[Relation] = elements.values.collect { case r: Relation ⇒ r }
  }

  case class Message(
    version: Float,
    generator: String,
    osm3s: OSM3S,
    elements: Seq[Element])

  import org.json4s._

  case class CaseInsensitiveTypeHints(hints: List[Class[_]]) extends TypeHints {
    def hintFor(clazz: Class[_]) = clazz.getName.substring(clazz.getName.lastIndexOf("$")+1).toLowerCase
    def classFor(hint: String) = hints find (h ⇒ hintFor(h) == hint.toLowerCase)
  }

  val osmResponseFormats = new DefaultFormats {
    override val typeHintFieldName: String = "type"
    override val typeHints = CaseInsensitiveTypeHints(
      List(
        classOf[Node], classOf[Relation], classOf[Way], classOf[Message],
        classOf[OSM3S], classOf[RelationshipMember], classOf[Element], classOf[RelationMember],
        classOf[NodeMember], classOf[WayMember]))
  }

  def parseAndExtract[A](s: String)(implicit mf: scala.reflect.Manifest[A]): A= {
    import org.json4s.native.Serialization.{read}
    import org.json4s.JsonAST.{JField, JString}
    import org.json4s.native.JsonParser

    implicit val formats = OSMResponseModel.osmResponseFormats

    val rawJson = JsonParser.parse(s, formats.wantsBigDecimal)
    val fixedJson = rawJson transformField {
      case JField("members", membersObject) ⇒ ("members", membersObject.transformField {
        case JField("type", JString(typeString)) ⇒ ("type", JString(typeString + "Member"))
      })
    }
    fixedJson extract(formats = formats, mf = mf)
  }

}
