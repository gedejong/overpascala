package nl.edejong.overpass


import scalaz._

trait AdditiveGroup[A] extends Monoid[A] {
  def negate(a: ⇒ A): A
}

trait AdditiveGroups extends MonoidLow {
  def additiveGroupFunction[A](f: A ⇒ A)(implicit sg: Semigroup[A], z: Zero[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
    override def append(s1: A, s2: => A) = sg.append (s1, s2)
    override val zero = z.zero
    override def negate(a: ⇒ A): A = f(a)
  }
}

object AdditiveGroup extends AdditiveGroups {
  implicit def OrderingAdditiveGroup: AdditiveGroup[Ordering] = additiveGroupFunction {
    case EQ ⇒ EQ
    case LT ⇒ GT
    case GT ⇒ LT
  }

  implicit def UnitAdditiveGroup: AdditiveGroup[Unit] = additiveGroupFunction(a ⇒ a)

  def negateF(a: Int): Int = -a

  import Scalaz._
  import Semigroup._
  import Zero._

  implicit def IntAdditiveGroup: AdditiveGroup[Int] = additiveGroupFunction(negateF)(IntSemigroup, IntZero)

  implicit def BooleanAdditiveGroup: AdditiveGroup[Boolean] = additiveGroupFunction(a ⇒ !a)

  implicit def ByteAdditiveGroup: AdditiveGroup[Byte] = additiveGroupFunction(a ⇒ (-a).toByte)

  implicit def ByteMultiplicationAdditiveGroup: AdditiveGroup[ByteMultiplication] =
    additiveGroupFunction((a: ByteMultiplication) ⇒ (-a).toByte ∏)

  implicit def LongAdditiveGroup: AdditiveGroup[Long] = additiveGroupFunction((a: Long) ⇒ (-a))(LongSemigroup, LongZero)

  implicit def LongMultiplicationAdditiveGroup: AdditiveGroup[LongMultiplication] =
    additiveGroupFunction(a ⇒ -a ∏)

  implicit def ShortAdditiveGroup: AdditiveGroup[Short] = additiveGroupFunction(a ⇒ (-a).toShort)

  // TODO
  //implicit def ShortMultiplicationAdditiveGroup: AdditiveGroup[ShortMultiplication] =
  //  additiveGroup((a ⇒ (-a).toShort) ∏)

  implicit def FloatAdditiveGroup: AdditiveGroup[Float] = additiveGroupFunction[Float](- _)(FloatSemigroup, FloatZero)

  implicit def DoubleAdditiveGroup: AdditiveGroup[Double] = additiveGroupFunction[Double](- _)(DoubleSemigroup, DoubleZero)

  implicit def BigIntegerAdditiveGroup: AdditiveGroup[java.math.BigInteger] =
    additiveGroupFunction(_.negate)

  // TODO: BigIntegerMultiplicationAdditiveGroup
  //implicit def BigIntegerMultiplicationAdditiveGroup: BigIntegerMultiplication =
   // additiveGroup((a: BigIntegerMultiplication) ⇒ a.negate() ∏)

  implicit def BigIntAdditiveGroup: AdditiveGroup[BigInt] = additiveGroupFunction(- _)

  implicit def BigIntMutliplicationAdditiveGroup: AdditiveGroup[BigIntMultiplication] =
    additiveGroupFunction(BigInt(1) / _ ∏)

  //implicit def ZipStreamAdditiveGroup[A]: AdditiveGroup[ZipStream[A]] = additiveGroup(zip(Stream.Empty))

  import AdditiveGroupOps._

  implicit def OptionAdditiveGroup[A](implicit ag: AdditiveGroup[A]): AdditiveGroup[Option[A]] =
    additiveGroupFunction(_.map(ag.negate(_)))

  implicit def Tuple2AdditiveGroup[A, B](implicit az: AdditiveGroup[A], bz: AdditiveGroup[B]) =
    additiveGroupFunction[(A, B)](t ⇒ (t._1.negate, t._2.negate))(Tuple2Semigroup, Tuple2Zero)

  /*
    implicit def FirstOptionAdditiveGroup[A]: AdditiveGroup[FirstOption[A]] = additiveGroup(None)

    implicit def LastOptionAdditiveGroup[A]: AdditiveGroup[LastOption[A]] = additiveGroup(None)

    implicit def LazyOptionAdditiveGroup[A]: AdditiveGroup[LazyOption[A]] = additiveGroup(LazyOption.none)

    implicit def FirstLazyOptionAdditiveGroup[A]: AdditiveGroup[FirstLazyOption[A]] = additiveGroup(LazyOption.none[A])

    implicit def LastLazyOptionAdditiveGroup[A]: AdditiveGroup[LastLazyOption[A]] = additiveGroup(LazyOption.none[A])

    implicit def ArrayAdditiveGroup[A: Manifest]: AdditiveGroup[Array[A]] = additiveGroup(new Array[A](0))

    implicit def EitherLeftAdditiveGroup[A, B](implicit bz: AdditiveGroup[B]): AdditiveGroup[Either.LeftProjection[A, B]] = additiveGroup(Right(∅[B]).left)

    implicit def EitherRightAdditiveGroup[A: AdditiveGroup, B]: AdditiveGroup[Either.RightProjection[A, B]] = additiveGroup(Left(∅[A]).right)

    implicit def EitherAdditiveGroup[A: AdditiveGroup, B]: AdditiveGroup[Either[A, B]] = additiveGroup(Left(∅[A]))

    implicit def MapAdditiveGroup[K, V: AdditiveGroup]: AdditiveGroup[Map[K, V]] = additiveGroup(Map.empty[K, V])

    implicit def IndSeqAdditiveGroup[A]: AdditiveGroup[IndSeq[A]] = additiveGroup(IndSeq.apply[A]())


    implicit def Tuple3AdditiveGroup[A, B, C](implicit az: AdditiveGroup[A], bz: AdditiveGroup[B], cz: AdditiveGroup[C]): AdditiveGroup[(A, B, C)] =
      additiveGroup((az.additiveGroup, bz.additiveGroup, cz.additiveGroup))

    implicit def Tuple4AdditiveGroup[A, B, C, D](implicit az: AdditiveGroup[A], bz: AdditiveGroup[B], cz: AdditiveGroup[C], dz: AdditiveGroup[D]): AdditiveGroup[(A, B, C, D)] =
      additiveGroup((az.additiveGroup, bz.additiveGroup, cz.additiveGroup, dz.additiveGroup))

    implicit def Function1ABAdditiveGroup[A, B: AdditiveGroup]: AdditiveGroup[A => B] = additiveGroup((_: A) => ∅[B])

    implicit def EndoAdditiveGroup[A]: AdditiveGroup[Endo[A]] = additiveGroup(EndoTo(identity(_: A)))

    implicit def DualAdditiveGroup[A: AdditiveGroup]: AdditiveGroup[Dual[A]] = additiveGroup(∅[A] σ)

    implicit def FingerTreeAdditiveGroup[V, A](implicit m: Reducer[A, V]): AdditiveGroup[FingerTree[V, A]] = {
      additiveGroup(FingerTree.empty)
    }

    implicit def AdditiveGroupKleisliAdditiveGroup[M[_], A, B](implicit z: AdditiveGroup[M[B]]): AdditiveGroup[Kleisli[M, A, B]] = additiveGroup(☆((_: A) => ∅[M[B]]))

    import scala.concurrent.Strategy
    import scala.concurrent.Strategy.Id

    implicit def StrategyAdditiveGroup[A]: AdditiveGroup[Strategy] = additiveGroup(Id)

    import java.util._
    import java.util.concurrent._

    implicit def JavaArrayListAdditiveGroup[A]: AdditiveGroup[ArrayList[A]] = additiveGroup(new ArrayList[A])

    implicit def JavaHashMapAdditiveGroup[K, V]: AdditiveGroup[HashMap[K, V]] = additiveGroup(new HashMap[K, V])

    implicit def JavaHashSetAdditiveGroup[A]: AdditiveGroup[HashSet[A]] = additiveGroup(new HashSet[A])

    implicit def JavaHashtableAdditiveGroup[K, V]: AdditiveGroup[Hashtable[K, V]] = additiveGroup(new Hashtable[K, V])

    implicit def JavaIdentityHashMapAdditiveGroup[K, V] = additiveGroup(new IdentityHashMap[K, V])

    implicit def JavaLinkedHashMapAdditiveGroup[K, V]: AdditiveGroup[LinkedHashMap[K, V]] = additiveGroup(new LinkedHashMap[K, V])

    implicit def JavaLinkedHashSetAdditiveGroup[A]: AdditiveGroup[LinkedHashSet[A]] = additiveGroup(new LinkedHashSet[A])

    implicit def JavaLinkedListAdditiveGroup[A]: AdditiveGroup[LinkedList[A]] = additiveGroup(new LinkedList[A])

    implicit def JavaPriorityQueueAdditiveGroup[A]: AdditiveGroup[PriorityQueue[A]] = additiveGroup(new PriorityQueue[A])

    implicit def JavaStackAdditiveGroup[A]: AdditiveGroup[Stack[A]] = additiveGroup(new Stack[A])

    implicit def JavaTreeMapAdditiveGroup[K, V]: AdditiveGroup[TreeMap[K, V]] = additiveGroup(new TreeMap[K, V])

    implicit def JavaTreeSetAdditiveGroup[A]: AdditiveGroup[TreeSet[A]] = additiveGroup(new TreeSet[A])

    implicit def JavaVectorAdditiveGroup[A]: AdditiveGroup[Vector[A]] = additiveGroup(new Vector[A])

    implicit def JavaWeakHashMapAdditiveGroup[K, V]: AdditiveGroup[WeakHashMap[K, V]] = additiveGroup(new WeakHashMap[K, V])

    implicit def JavaArrayBlockingQueueAdditiveGroup[A]: AdditiveGroup[ArrayBlockingQueue[A]] = additiveGroup(new ArrayBlockingQueue[A](0))

    implicit def JavaConcurrentHashMapAdditiveGroup[K, V]: AdditiveGroup[ConcurrentHashMap[K, V]] = additiveGroup(new ConcurrentHashMap[K, V])

    implicit def JavaConcurrentLinkedQueueAdditiveGroup[A]: AdditiveGroup[ConcurrentLinkedQueue[A]] = additiveGroup(new ConcurrentLinkedQueue[A])

    implicit def JavaCopyOnWriteArrayListAdditiveGroup[A]: AdditiveGroup[CopyOnWriteArrayList[A]] = additiveGroup(new CopyOnWriteArrayList[A])

    implicit def JavaCopyOnWriteArraySetAdditiveGroup[A]: AdditiveGroup[CopyOnWriteArraySet[A]] = additiveGroup(new CopyOnWriteArraySet[A])

    implicit def JavaLinkedBlockingQueueAdditiveGroup[A]: AdditiveGroup[LinkedBlockingQueue[A]] = additiveGroup(new LinkedBlockingQueue[A])

    implicit def JavaSynchronousQueueAdditiveGroup[A]: AdditiveGroup[SynchronousQueue[A]] = additiveGroup(new SynchronousQueue[A])
    */
}

trait AdditiveGroupOps[A] {
  def value: A

  import Scalaz._

  def |-|(a: A)(implicit ag: AdditiveGroup[A]): A = value |+| (ag negate a)

  def negate(implicit ag: AdditiveGroup[A]): A = ag negate value
}

object AdditiveGroupOps {
  implicit def toAdditiveGroupOps[A](a: A): AdditiveGroupOps[A] = new AdditiveGroupOps[A] {
    def value = a
  }
}

