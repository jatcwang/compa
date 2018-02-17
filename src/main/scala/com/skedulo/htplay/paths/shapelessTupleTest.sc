import shapeless._
import syntax.std.product._
import syntax.std.tuple._

type MyHList = String :: Int :: HNil
val f: MyHList = "adsf" :: 5 :: HNil

def gen[Tup, T <: HList](in: T)(implicit Gen: Generic.Aux[Tup, T]): Tup = {
  Gen.from(in)
}

gen[(String, Int), MyHList](f)

val g = implicitly[Generic.Aux[(Int, String), Int :: String :: HNil]]

