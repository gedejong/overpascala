package nl.edejong.overpass

import scalaz.Monoid
import scalaz._
import Scalaz._
import AdditiveGroupOps._

trait VectorSpace[V, S] extends AdditiveGroup[V] {
  def mult(v: V, s: ⇒ S): V
}

abstract class VectorSpaces {
  def vectorspace[V, S](multF: (V, S) ⇒ V)(implicit a: AdditiveGroup[V]): VectorSpace[V, S] = new VectorSpace[V, S] {
    override def append(s1: V, s2: ⇒ V) = a append (s1, s2)
    override val zero = a.zero
    override def negate(v: ⇒ V): V = a negate v
    override def mult(v: V, s: ⇒ S): V = multF(v, s)
  }
}

object VectorSpace extends VectorSpaces {
  import AdditiveGroup._

  implicit def DoubleVectorSpace: VectorSpace[Double, Double] = vectorspace[Double, Double](_ * _)(DoubleAdditiveGroup)

  implicit def LongVectorSpace: VectorSpace[Long, Long] = vectorspace[Long, Long](_ * _)(LongAdditiveGroup)

  implicit def IntVectorSpace: VectorSpace[Int, Int] = vectorspace[Int, Int](_ * _)(IntAdditiveGroup)

  implicit def FunctorVectorSpace[V, S, F[_]](implicit vs: VectorSpace[V,S], f: Functor[F]): VectorSpace[F[V], S] =
    vectorspace((vOpt, s) ⇒ vOpt map (vs.mult(_, s)))

  // Please fix me, I don't think this one is really a vector space, but I need it for some examples.
  implicit def LongDoubleVectorSpace: VectorSpace[Long, Double] = vectorspace[Long, Double]((l, d) ⇒ (l * d).toLong)(LongAdditiveGroup)

  import VectorSpaceOps._
  implicit def Tuple2VectorSpace[A1, A2, P](implicit as1: VectorSpace[A1, P], as2: VectorSpace[A2, P]): VectorSpace[(A1, A2), P] =
    vectorspace[(A1, A2), P]((a1, p) ⇒ (a1._1 |*| p, a1._2 |*| p))(Tuple2AdditiveGroup)
}

trait VectorSpaceOps[V] {
  def value: V

  def |*|[S](s: S)(implicit v: VectorSpace[V, S]): V = v.mult(value, s)

  import VectorSpaceOps._
  def lerp[S](v2: V)(s: S)(implicit v: VectorSpace[V, S]): V = value |+| ((v2 |-| value) |*| s)
}

object VectorSpaceOps {
  implicit def toVectorSpaceOps[V](v: V): VectorSpaceOps[V] = new VectorSpaceOps[V] {
    def value = v
  }
}

