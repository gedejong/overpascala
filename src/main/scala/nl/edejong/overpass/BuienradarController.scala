package nl.edejong.overpass

import org.joda.time.{LocalTime, DateTime}

import scalaz.geo.Coord
import dispatch._, Defaults._
import scalaz._
import Scalaz._

object BuienradarController {
  object BuienRadarParser extends (Res => Option[List[(Int, DateTime)]]) {
    def apply(res: Res) = parseResult(res.getResponseBody)
  }

  private final val elementRegex = """(\d{3})\|(\d{2}:\d{2})""".r

  def parseTuple(str: String): Option[(Int, DateTime)] = str match {
    case elementRegex(rainIntensity, time) ⇒
      val dateTime = new LocalTime(time).toDateTimeToday
      val tuple = (rainIntensity.toInt, dateTime)
      Some(tuple)
    case _ ⇒ None //(s"Could not read buienradar result: $subresult")
  }

  def parseResult(body: String): Option[List[(Int, DateTime)]] = {
    body
      .split("\\s+")
      .toList
      .map(str => parseTuple(str))
      .sequence
  }

  def krijgRegen(coord: Coord) =
    Http(url("http://gps.buienradar.nl/getrr.php") <<?
      Map(
        "lon" → coord.longitude.value.toString,
        "lat" → coord.latitude.value.toString) OK BuienRadarParser)
}
