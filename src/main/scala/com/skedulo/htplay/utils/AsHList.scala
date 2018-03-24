package com.skedulo.htplay.utils

import shapeless._

// Given any type A we want an HList representation of it
// For any HList, return itself, otherwise return the type 'A :: HNil'
trait AsHList[A] extends DepFn1[A] {
  override type Out <: HList
}

object AsHList extends HigherPriorityAsHList {
  type Aux[A, Out0 <: HList] = AsHList[A] { type Out = Out0 }
}

trait HigherPriorityAsHList extends LowerPriorityAsHList {
  implicit def hlistAsHList[A <: HList] = new AsHList[A] {
    override type Out = A
    override def apply(t: A): Out = t
  }
}

trait LowerPriorityAsHList {
  implicit def anyAsHList[A] = new AsHList[A] {
    override type Out = A :: HNil
    override def apply(t: A): Out = t :: HNil
  }
}

