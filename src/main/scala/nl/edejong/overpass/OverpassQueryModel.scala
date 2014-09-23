package nl.edejong.overpass

import java.text.{ParsePosition, FieldPosition, NumberFormat}

import scalaz.Scalaz._
import scalaz.Show._
import scalaz.Show
import scalaz.geo.{Coord, Latitude, Longitude}
import io.github.karols.units._
import io.github.karols.units.defining._

object OverpassQueryModel {
  type metre = DefineUnit[_m]
  type kilometre = DefineUnit[_k~:_m]
  implicit val km_to_m = one[kilometre].contains(1000)[metre]

  sealed trait Statement

  case object OutStatement extends Statement

  sealed trait PipeableStatement extends Statement {
    def |(other: Statement): PipedStatement = PipedStatement(this, other)
    def into(variable: String): IntoStatement = IntoStatement(this, variable)
  }
  case class PipedStatement(left: Statement, right: Statement) extends PipeableStatement
  case class IntoStatement(s: Statement, variable: String) extends PipeableStatement
  sealed trait QueryStatement extends PipeableStatement {
    def apply(c: Clause) = ClausedQueryStatement(this, c)
  }
  case object Node extends QueryStatement
  case object Relation extends QueryStatement
  case object Way extends QueryStatement
  case class Union(s: Statement) extends QueryStatement
  case object >> extends QueryStatement
  case class ClausedQueryStatement(qs: QueryStatement, clause: Clause) extends QueryStatement

  sealed trait Clause {
    def and(right: Clause): Clause = AndClause(this, right)
  }

  case class AndClause(left: Clause, right: Clause) extends Clause
  case class BoundingBox(topLeft: Coord, bottomRight: Coord) extends Clause
  case class Id(id: Long) extends Clause

  sealed trait TagRequest extends Clause

  case class Eq(tag: String, value: String) extends TagRequest {
    def ~! = Neq(tag, value)
  }
  case class HasKey(tag: String) extends TagRequest
  case class Regex(tag: String, value: String) extends TagRequest {
    def unary_! = NotRegex(tag, value)
  }
  case class Neq(tag: String, value: String) extends TagRequest
  case class NotRegex(tag: String, value: String) extends TagRequest

  case class Around(dist: IntU[metre]) extends TagRequest

  sealed trait Direction
  case object Forward extends Direction
  case object Backward extends Direction

  sealed trait RecursiveKind
  case object Nodes extends RecursiveKind
  case object Ways extends RecursiveKind
  case object Relations extends RecursiveKind

  case class Membership(recursiveKind: RecursiveKind, direction: Direction) extends Clause

  def containedIn(recursiveKind: RecursiveKind) = Membership(recursiveKind, Forward)
  def isPartOf(recursiveKind: RecursiveKind) = Membership(recursiveKind, Backward)

  implicit class PimpedStringClause(str: String) {
    def ===(other: String) = Eq(str, other)
    def !==(other: String) = Neq(str, other)
    def ==~(other: String) = Regex(str, other)
    def !=~(other: String) = Regex(str, other)
  }

  implicit class PimpedCoordClause(c: Coord) {
    def to(other: Coord) = BoundingBox(c, other)
  }

  implicit def LongitudeShow: Show[Longitude] = shows(_.value.shows)
  implicit def LatitudeShow: Show[Latitude] = shows(_.value.shows)
  implicit def CoordShow: Show[Coord] = shows((c: Coord) => c.latitude.shows + ", " + c.longitude.shows)

  implicit def StatementShow[S <: Statement]: Show[S] = shows {
    case OutStatement ⇒ "out"
    case ClausedQueryStatement(qs, clause) ⇒ qs.shows + clause.shows
    case PipedStatement(left, right) ⇒ left.shows + ";" + right.shows
    case Node ⇒ "node"
    case Relation ⇒ "rel"
    case Way ⇒ "way"
    case >> ⇒ ">>"
    case Union(s) ⇒ "(" + s.shows + ";)"
    case IntoStatement(s, variable) ⇒ s.shows + "->." + variable
  }

  implicit def ClauseShows[S <: Clause]: Show[S] = shows {
    case Id(id) ⇒ s"($id)"
    case Eq(tag, value) ⇒ s"""["$tag"="$value"]"""
    case HasKey(tag) ⇒ """["$tag"]"""
    case Regex(tag, value) ⇒ s"""["$tag"~"$value"]"""
    case Neq(tag, value) ⇒ s"""["$tag"!="$value"]"""
    case NotRegex(tag, value) ⇒ s"""["$tag"!~"$value"]"""
    case BoundingBox(topLeft, bottomRight) ⇒ s"""(${topLeft.shows}, ${bottomRight.shows})"""
    case AndClause(left, right) ⇒ left.shows + right.shows
    case Around(dist) ⇒ s"(around: ${dist.value})"
    case Membership(kind, direction) ⇒ "(" + direction.shows + kind.shows + ")"
  }

  implicit def RecursiveKindShows[S <: RecursiveKind]: Show[S] = shows {
    case Nodes ⇒ "n"
    case Ways ⇒ "w"
    case Relations ⇒ "r"
  }

  implicit def DirectionShows[S <: Direction]: Show[S] = shows {
    case Forward ⇒ ""
    case Backward ⇒ "b"
  }
}
