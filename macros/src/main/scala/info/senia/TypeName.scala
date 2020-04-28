package info.senia

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait TypeName[T] {
  val name: String
}

object TypeName {
  implicit def materializeTypeName[T]: TypeName[T] = macro typeNameImpl[T]

  def typeNameImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val T = c.weakTypeOf[T]
    val name = T.typeSymbol.name.toString
    q" new _root_.info.senia.TypeName[$T]{ val name: String = $name } "
  }
}
