package info.senia

import shapeless._
import shapeless.labelled._
import shapeless.syntax.singleton._
import shapeless.record._

import scala.reflect.ClassTag

object Main2 {

  trait Show[-T] {
    def show(t: T): String
  }

  object Show {
    implicit val showString: Show[String] = s => s"string($s)"
    implicit val intString: Show[Int] = i => s"int($i)"
    implicit val longString: Show[Long] = l => s"long($l)"
    implicit val doubleString: Show[Double] = d => s"double($d)"

    implicit def fromClass[T](implicit cs: ClassShow[T]): Show[T] = cs.show
  }

  trait HListShow[L <: HList] {
    def apply(l: L): List[String]
  }

  object HListShow {
    implicit val caseHNil: HListShow[HNil] = _ => Nil
    implicit def caseHCons[K <: Symbol, V, T <: HList](
      implicit key: Witness.Aux[K],
      sv: Show[V],
      next: HListShow[T]
    ): HListShow[FieldType[K, V] :: T] =
      l => s"${key.value.name} = ${l.head.show}" :: next(l.tail)
  }

  trait ClassShow[T] {
    val show: Show[T]
    def cast(a: Any): Option[T]
    def unapply(a: Any): Option[String] = cast(a).map(show.show)
  }

  object ClassShow {
    implicit def onT[T, Repr <: HList](
      implicit gen: LabelledGeneric.Aux[T, Repr],
      hs: HListShow[Repr],
      ct: ClassTag[T],
      tn: TypeName[T]
    ): ClassShow[T] = new ClassShow[T] {
      val show: Show[T] = t => {
        val fields = hs(gen.to(t))
        if (fields.isEmpty) tn.name
        else s"${tn.name}(${fields.mkString(", ")})"
      }

      def cast(a: Any): Option[T] = a match {
        case t: T => Some(t)
        case _    => None
      }
    }

    implicit def onSealed[T, Repr <: Coproduct](
      implicit gen: Generic.Aux[T, Repr],
      cc: CollectCases[Repr],
      ct: ClassTag[T]
    ): ClassShow[T] = new ClassShow[T] {
      val show: Show[T] = {
        val cases = cc()

        t =>
          cases.view.flatMap(_.unapply(t)).head
      }

      def cast(a: Any): Option[T] = a match {
        case t: T => Some(t)
        case _    => None
      }
    }
  }

  trait CollectCases[G <: Coproduct] {
    def apply(): List[ClassShow[_]]
  }

  object CollectCases {
    implicit val caseCNil: CollectCases[CNil] = () => Nil
    implicit def caseCCons[H, T <: Coproduct](
      implicit cs: ClassShow[H],
      next: CollectCases[T]
    ): CollectCases[H :+: T] =
      () => cs :: next()
  }

  implicit class ShowOps[T](val t: T) extends AnyVal {
    def show(implicit ev: Show[T]): String = ev.show(t)
  }

  sealed trait X
  case object A extends X
  case class B() extends X
  case class C(s: String) extends X

  case class Person(firstName: String, lastName: String, age: Int)

  sealed trait MyEnum
  object MyEnum {
    case object Case1 extends MyEnum
    case object Case2 extends MyEnum
    case class Case3() extends MyEnum
  }

  trait CollectEnum[S, G <: Coproduct] {
    def apply(): List[(String, S)]
  }

  trait CollectEnumLowPriority {
    implicit def caseIgnore[S, H, T <: Coproduct](
      implicit next: CollectEnum[S, T]
    ): CollectEnum[S, H :+: T] =
      () => next()
  }

  object CollectEnum extends CollectEnumLowPriority {
    implicit def caseCNil[S]: CollectEnum[S, CNil] = () => Nil
    implicit def caseCCons[S, K <: Symbol, V <: S, T <: Coproduct](
      implicit get: Generic.Aux[V, HNil],
      key: Witness.Aux[K],
      next: CollectEnum[S, T]
    ): CollectEnum[S, FieldType[K, V] :+: T] =
      () => (key.value.name -> get.from(HNil)) :: next()
  }

  trait EnumValuesHelper[S] {
    def apply[Repr <: Coproduct]()(
      implicit gen: LabelledGeneric.Aux[S, Repr],
      ce: CollectEnum[S, Repr]
    ): List[(String, Any)] = {
      val _ = gen
      ce()
    }
  }

  def enumValues[S]: EnumValuesHelper[S] = new EnumValuesHelper[S] {}

  def main(args: Array[String]): Unit = {

    println(13d.show)

    println(Person("John", "Doe", 33).show)

    val x: X = A

    println(x.show)
    println(B().show)
    println(C("str").show)

    case class Inner(s: String)
    case class Outer(inner: Inner)

    println(Outer(Inner("smthng")).show)

    println(enumValues[MyEnum]())
    println(enumValues[X]())

    val a =
      ("a" ->> 1) ::
        ("b" ->> "str") ::
        HNil

    println(a("b"))

    val b = ("a" ->> 1, "b" ->> "str")
  }

}
