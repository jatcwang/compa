package com.skedulo.htplay.paths

import org.http4s.{Query, Request, Response}
import shapeless.{HList, HNil}
import shapeless._
import shapeless.ops.hlist.{Prepend, Tupler}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converter.{AnyConverter, ExistConverter}
import shapeless.ops.traversable.FromTraversable

import scala.collection.{mutable => mut}
import shapeless.syntax.std.traversable._

import scala.language.higherKinds

sealed trait Segment

// Literal matching of a segment
case class LiteralSegment(str: String) extends Segment
// a path variable may convert to multiple parameters
//TODOO: make you able to pass a name
case class PathVarSegment[Err, A](parser: String => Either[Err, A]) extends Segment

//TODOO: make private
case class PBuilder[Err, Vars <: HList](matchSegments: Vector[Segment], converters: Vector[ExistConverter[Err]]) {

  def /(segment: String): PBuilder[Err, Vars] = {
    PBuilder[Err, Vars](matchSegments :+ LiteralSegment(segment), converters)
  }

  def /[A](segment: PathVarSegment[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): PBuilder[Err, prepend.Out] = {
    val c = StringConverter(segment.parser)
    PBuilder(matchSegments :+ segment, converters :+ c)
  }

  def :?[A](queryParam: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[Err, prepend.Out] = {
    this.toQBuilder.withQueryParam(queryParam)(prepend)
  }

  private def toQBuilder: QBuilder[Err, Vars] = QBuilder(matchSegments, converters)

  def make[F[_]](implicit t: FromTraversable[Vars]): Matcher[F, Vars] = {
    Matchers.makeMatcher[F, Err, Vars](converters, matchSegments)
  }

}

object PBuilder {
  val root: PBuilder[String, HNil] = PBuilder(
    matchSegments = Vector.empty,
    converters = Vector.empty
  )

  val intVar: PathVarSegment[String, Int] = PathVarSegment(str => Either.catchNonFatal(str.toInt).leftMap(_.getMessage))
  val stringVar: PathVarSegment[String, String] = PathVarSegment(str => Right(str))
}

sealed trait Converter[In, Err, A] {
  def converter: In => Either[Err, A]
}

object Converter {
  type ExistConverter[Err] = Converter[_, Err, _]
  type AnyConverter[Err] = Converter[Any, Err, Any]
}

// Convert a string into A
case class StringConverter[Err, A](override val converter: String => Either[Err, A]) extends Converter[String, Err, A]
case class QueryStringConverter[Err, A](override val converter: Query => Either[Err, A]) extends Converter[Query, Err, A]
case class RequestConverter[F[_], Err, A](override val converter: Request[F] => Either[Err, A]) extends Converter[Request[F], Err, A]
