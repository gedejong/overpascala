package nl.edejong.overpass

import dispatch._, Defaults._
import nl.edejong.overpass.OSMResponseModel.Node
import nl.edejong.overpass.OverpassQueryModel.Node
import nl.edejong.overpass.OverpassQueryModel._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scalaz.geo.Geo._
import io.github.karols.units._

class OverpassQueryModelTest extends FlatSpec with Matchers {
  "The BuienradarController" should "give us some results" in {
    val coordinate = latitude(1.1) |:| longitude(1.2)
    import scala.concurrent.duration._

    println(Await.result(BuienradarController.krijgRegen(coordinate), 20 seconds))
  }

  "The BuienradarController" should "correctly parse results" in {
    val input = """000|17:35
                  |000|17:40
                  |000|17:45
                  |000|17:50
                  |000|17:55
                  |000|18:00
                  |000|18:05
                  |000|18:10
                  |000|18:15
                  |000|18:20
                  |000|18:25
                  |000|18:30
                  |000|18:35
                  |000|18:40
                  |000|18:45
                  |000|18:50
                  |000|18:55
                  |000|19:00
                  |000|19:05
                  |000|19:10
                  |000|19:15
                  |000|19:20
                  |000|19:25
                  |000|19:30
                  |000|19:35
                  |""".stripMargin
    BuienradarController.parseResult(input)
  }
  import OverpassQueryInterface._

  "The OverPassQueryModel" should "show a simple example" in {

    val deventerQueryStatement = Node(
      "addr:city" === "Deventer" and
        "addr:housenumber" === "25" and
        "addr:street" === "Singel") | OutStatement

    val future = for (deventerResponse ← query(deventerQueryStatement)) yield {
      deventerResponse.nodes.head
    }
    import scala.concurrent.duration._

    println(Await.result(future, 5 seconds))
  }

  "The overpassQueryModel" should "do some neat tricks" in {

    val deventerQueryStatement = Node(
      "addr:city" === "Deventer" and
      "addr:housenumber" === "25" and
      "addr:street" === "Singel") | OutStatement

    val result =
      for {
        deventerResponse ← query(deventerQueryStatement)
        result ← query(
          Node(Id(deventerResponse.nodes.head.id)) |
          Way(Around(1.kilo[metre])) |
          Union(
            Relation(isPartOf(Ways) and "route" === "bicycle" and "type" === "route") |
              Way(containedIn(Relations)) |
              Node(containedIn(Ways))
          ) | OutStatement)
      } yield {
        implicit val resultSet = result

        // For all relationships found
        for (rel ← result.rels) yield {
          // Retrieve all the ways connected to the relationship

          val nodes = rel.wayMembers collect {
            case Left(w) ⇒ w.findNodes collect {
              case Left(n) ⇒ n
            }
          } flatten

          // val lonLat = nodes.map(node ⇒ (node.lon, node.lat))

          val deventerNode = deventerResponse.nodes.head

          val closestNode = nodes minBy (node ⇒ (node.lon - deventerNode.lon).pow2 + (node.lat - deventerNode.lat).pow2)
          val index = nodes.indexOf(closestNode)
          val (beforeNodes, afterNodes) = nodes.splitAt(index)
          val nodesPlusDistBefore = seqDist(beforeNodes.reverse)
          val nodesPlusDistAfter = seqDist(afterNodes)
          (nodesPlusDistBefore.last._2, nodesPlusDistAfter.last._2)
        }
      }
    import scala.concurrent.duration._

    println(Await.result(result, 60 seconds))
  }

  import OSMResponseModel._

  def seqDist(s: Seq[Node]) = s.foldLeft[Option[(Node, Long)]](None)(accDist)

  def accDist(prev: Option[(Node, Long)], node: Node): Option[(Node, Long)] = {
    prev match {
      case None ⇒ Some((node, 0L))
      case Some((prevNode, sumDist)) ⇒ Some((node, dist(prevNode, node) + sumDist))
    }

  }

  // El cheapo distance
  def dist(node: Node, prevNode: Node): Long = {
    val lat1 = node.lat
    val lon1 = node.lat
    val lat2 = prevNode.lat
    val lon2 = prevNode.lon
    val R = 6371
    val φ1 = lat1.toRadians
    val φ2 = lat2.toRadians
    val Δφ = (lat2-lat1).toRadians
    val Δλ = (lon2-lon1).toRadians

    val a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
      Math.cos(φ1) * Math.cos(φ2) *
        Math.sin(Δλ/2) * Math.sin(Δλ/2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))

    R * c
  }


}
