import shapeless._
import syntax.std.product._
import syntax.std.tuple._
import shapeless.poly._

type MyHList = String :: Int :: HNil
val f: MyHList = "adsf" :: 5 :: HNil


val v: Set[String] :: Set[Int] :: HNil = Set("adsf") :: Set(34) :: HNil

//def gen[Tup, T <: HList](in: T)(implicit Gen: Generic.Aux[Tup, T]): Tup = {
//  Gen.from(in)
//}
//
//gen[(String, Int), MyHList](f)
//
//val g = implicitly[Generic.Aux[(Int, String), Int :: String :: HNil]]

class Boo extends (Set ~> Id) {
  override def apply[T](f: Set[T]): T = f.headOption.get
}

val x = new Boo

import x.caseUniv

v.map(x)

