package com.skedulo.htplay.paths

import org.http4s.{Request, Response}
import shapeless.{HList, HNil}
import shapeless._
import shapeless.ops.hlist.{Prepend, Tupler}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converters.{AnyConverter, ExistConverter}
import monix.eval.Task
import shapeless.ops.traversable.FromTraversable

import scala.collection.{mutable => mut}
import shapeless.syntax.std.traversable._

import scala.language.higherKinds

sealed trait QueryParam[Err, T]
//TODOO: do we need this? we can probably generalize this to use MultiParam (and call headoption)
case class SingleParam[Err, T](key: String, convert: String => Either[Err, T]) extends QueryParam[Err, T]
// indicate that we shouldn't ignore multiple params
case class MultiParam[Err, T](key: String, convert: Seq[String] => Either[Err, T]) extends QueryParam[Err, T]

object QueryParam {
  def int(key: String) = SingleParam[String, Int](key, str => Either.catchNonFatal(str.toInt).leftMap(_.getMessage))
  def str(key: String) = SingleParam[String, String](key, str => Right(str))
}

//TODOO: make private
case class QBuilder[Err, Vars <: HList](matchSegments: Vector[Segment], converters: Vector[ExistConverter[Err]]) {

  def withQueryParam[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[Err, prepend.Out] = {
    val converter = QueryStringConverter(q => {
      //TODOO: handle empty
      val paramValue = q.params.get(param.key).get
      param.convert(paramValue)
    })
    QBuilder(matchSegments, converters :+ converter)
  }

  def &[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[Err, prepend.Out] = {
    // pass in the same implicit so compiler can prove that the return type
    // is the same (due to path-dependent types)
    withQueryParam(param)(prepend)
  }
}


