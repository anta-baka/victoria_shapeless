package info.senia

import shapeless.labelled.FieldType

import scala.collection.immutable
import shapeless.syntax.singleton._

object Main {
  sealed trait HList
  case object HNil extends HList
  type HNil = HNil.type
  case class ::[H, T <: HList](head: H, tail: T) extends HList {
    override def toString: String = s"$head :: $tail"
  }
  val Cons: ::.type = ::

  implicit class HListOps[L <: HList](l: L) {
    def ::[T](t: T): T :: L = Cons(t, l)
    def reverse(implicit r: ReverseHelper[L, HNil]): r.Out = r(l, HNil)
    def apply[T <: Singleton](t: T)(implicit hGet: HListGet[T, L]): hGet.Out = {
      val _ = t
      hGet(l)
    }
  }

  /*
   * [1 2 3 4 5]    []
   *
   *
   * [3 4 5]    [2 1]
   *
   * [] A = A
   * */
  def reverse[T](l: List[T], agg: List[T] = Nil): List[T] = l match {
    case immutable.::(head, next) => reverse(next, head :: agg)
    case Nil                      => agg
  }

  trait Reverse[L <: HList] {
    type Out <: HList
    def apply(l: L): Out
  }

  trait ReverseHelper[L <: HList, A <: HList] {
    type Out <: HList
    def apply(l: L, a: A): Out
  }

  object Reverse {
    type Aux[L <: HList, Out0 <: HList] = Reverse[L] { type Out = Out0 }

    implicit def fromHelper[T <: HList](
      l: T
    )(implicit h: ReverseHelper[T, HNil]): Aux[T, h.Out] = new Reverse[T] {
      type Out = h.Out
      def apply(l: T): h.Out = h(l, HNil)
    }
  }

  object ReverseHelper {
    type Aux[L <: HList, A <: HList, Out0 <: HList] = ReverseHelper[L, A] {
      type Out = Out0
    }

    implicit def caseHNil[A <: HList]: Aux[HNil, A, A] =
      new ReverseHelper[HNil, A] {
        type Out = A
        def apply(l: HNil, a: A): A = a
      }

    implicit def caseCons[H, T <: HList, A <: HList](
      implicit next: ReverseHelper[T, H :: A]
    ): Aux[H :: T, A, next.Out] =
      new ReverseHelper[H :: T, A] {
        type Out = next.Out
        def apply(l: H :: T, a: A): next.Out = next(l.tail, l.head :: a)
      }
  }

  trait HListGet[T, L <: HList] {
    type Out
    def apply(l: L): Out
  }

  trait HListGetLowPriority {
    implicit def caseNext[K, H, T <: HList](
      implicit next: HListGet[K, T]
    ): HListGet.Aux[K, H :: T, next.Out] =
      new HListGet[K, H :: T] {
        type Out = next.Out
        def apply(l: H :: T): Out = next(l.tail)
      }
  }

  object HListGet extends HListGetLowPriority {
    type Aux[T, L <: HList, Out0] = HListGet[T, L] { type Out = Out0 }

    implicit def found[K, V, T <: HList]: Aux[K, FieldType[K, V] :: T, V] =
      new HListGet[K, FieldType[K, V] :: T] {
        type Out = V
        def apply(l: FieldType[K, V] :: T): V = l.head
      }
  }

  def main(args: Array[String]): Unit = {
    val l
      : Int :: String :: Long :: Double :: Main.HNil.type = 1 :: "str" :: 666L :: 13d :: HNil

    val r = l.reverse

    println(r)

    val record =
      ("a" ->> 1) ::
        ("b" ->> "str") ::
        ("c" ->> 666L) ::
        HNil

    val x = record("a")
    val x2: Int = x
    println(x2)
  }

}
