package nl.edejong.overpass

import dispatch.{Http, as, url}
import nl.edejong.overpass.OSMResponseModel._
import nl.edejong.overpass.OverpassQueryModel.Statement
import scalaz.Scalaz._

import scala.concurrent.{ExecutionContext, Future}

object OverpassQueryInterface {
  def query(stm: Statement)(implicit executionContext: ExecutionContext): Future[ResultSet] = {
    println(stm.shows)
    val query = url("http://overpass.osm.rambler.ru/cgi/interpreter") <<? Map("data" → s"[out:json];${stm.shows};")
    for (result ← Http(query OK as.String)) yield {
      val elements = parseAndExtract[Message](result).elements
      elements.foldLeft(ResultSet())(_ augment _)
    }
  }

}
