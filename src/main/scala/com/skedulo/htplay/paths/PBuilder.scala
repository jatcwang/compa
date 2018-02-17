package com.skedulo.htplay.paths

import shapeless.HList
import shapeless.ops.hlist.Prepend

sealed trait Segment

case class StringSegment(str: String) extends Segment
// a path variable may convert to multiple parameters
case class PathVarSegment[Err, A <: HList](parser: A => Either[Err, A]) extends Segment

case class PBuilder[Err, Vars <: HList](segments: Vector[Segment]) {
  def /(segment: StringSegment): PBuilder[Err, Vars] = {
    PBuilder[Err, Vars](segments :+ segment)
  }

  def /[SegVars <: HList](segment: PathVarSegment[Err, SegVars])(implicit prepend: Prepend[Vars, SegVars]): PBuilder[Err, prepend.Out] = {
    ???
  }
}

object PBuilder {

}

/*
root / 'lol / 'hello

  */

/*
Matching:
  count segments, filter out any RequestMatcher that doesn't have the right number of segments
 */
