package nl.edejong.overpass

import org.joda.time.DateTime

import scalaz.Scalaz._
import scalaz.geo.{GeodeticCurve, Coord}
import scalaz.{Endo, Semigroup, Scalaz}

trait AffineSpace[A, P] {
  type Diff = VectorSpace[P, _]

  def minus(a1: A, a2: A): P
  def plus(a: A, v: P): A
}

abstract class AffineSpaces {
  def affineSpace[A, P](minusF: (A, A) ⇒ P, plusF: (A, P) ⇒ A) = new AffineSpace[A, P]{
    def minus(a1: A, a2: A): P = minusF(a1, a2)
    def plus(a: A, v: P): A = plusF(a, v)
  }
}

object AffineSpace extends AffineSpaces {
  implicit def IntegerAffineSpace: AffineSpace[Int, Int] = affineSpace(_ - _, _ + _)
  implicit def LongAffineSpace: AffineSpace[Long, Long] = affineSpace(_ - _, _ + _)
  implicit def DoubleAffineSpace: AffineSpace[Double, Double] = affineSpace(_ - _, _ + _)
  implicit def FloatAffineSpace: AffineSpace[Float, Float] = affineSpace(_ - _, _ + _)
  implicit def DateTimeAffineSpace: AffineSpace[DateTime, Long] =
    affineSpace(_.getMillis - _.getMillis, (dt, millis) ⇒ new DateTime(dt.getMillis + millis))

  import AffineSpaceOps._

  implicit def Tuple2AffineSpace[A1, A2, P1, P2](implicit as1: AffineSpace[A1, P1], as2: AffineSpace[A2, P2]): AffineSpace[(A1, A2), (P1, P2)] =
    affineSpace(
      (a1, a2) ⇒ (a1._1 |-| a2._1, a1._2 |-| a2._2),
      (a1, v) ⇒ (a1._1 |+| v._1, a1._2 |+| v._2))

  implicit def Tuple3AffineSpace[A1, A2, A3, P1, P2, P3](implicit as1: AffineSpace[A1, P1], as2: AffineSpace[A2, P2], as3: AffineSpace[A3, P3]): AffineSpace[(A1, A2, A3), (P1, P2, P3)] =
    new AffineSpace[(A1, A2, A3), (P1, P2, P3)] {
      import AffineSpaceOps._

      def minus(a1: (A1, A2, A3), a2: (A1, A2, A3)) = (a1._1 |-| a2._1, a1._2 |-| a2._2, a1._3 |-| a2._3)
      def plus(a1: (A1, A2, A3), v: (P1, P2, P3)) = (a1._1 |+| v._1, a1._2 |+| v._2, a1._3 |+| v._3)
    }

}

trait AffineSpaceOps[A] {
  def value: A

  import AffineSpaceOps._
  import VectorSpaceOps._

  def |-|[V](a: A)(implicit v: AffineSpace[A, V]): V = v minus (value, a)
  def |+|[V](other: V)(implicit v: AffineSpace[A, V]): A = v plus (value, other)

  // Affine linear interpolation.
  def alerp[S, T](v2: A)(t: T)(implicit as: AffineSpace[A, S], vs: VectorSpace[S, T]): A = {
    val diff: S = v2 |-| value
    val scalarDiff: S = diff |*| t
    value |+| scalarDiff
  }
}

object AffineSpaceOps {
  implicit def toAffineSpaceOps[V](v: V): AffineSpaceOps[V] = new AffineSpaceOps[V] {
    def value = v
  }
}

