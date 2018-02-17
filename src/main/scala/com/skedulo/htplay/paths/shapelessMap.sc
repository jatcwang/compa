import shapeless._
import shapeless.ops.hlist.{IsHCons, Prepend}
import shapeless.ops.traversable.FromTraversable
import syntax.std.product._
import syntax.std.tuple._
import shapeless.poly._
import syntax.std.traversable._

type XX = Int :: String :: HNil

val v: XX = 23 :: "asdf" :: HNil

v.toList

val pp =  Prepend[Int :: HNil, String :: HNil]

val t: pp.Out = 32 :: "asdf" :: HNil

//def doStuff[IL <: HList, RL <: HList](results: Vector[Any], inputs: IL, funcs: List[Any => Any])
//  (implicit f: FromTraversable[RL],
//  isHCons: IsHCons[IL]): RL = {
//  funcs match {
//    case List() => results.toHList[RL].get
//    case f +: restFuncs => {
//      val thisResult = f.apply(inputs.head)
//      val allResults = results :+ thisResult
//      doStuff(allResults, inputs.tail, restFuncs)
//    }
//  }
//}
