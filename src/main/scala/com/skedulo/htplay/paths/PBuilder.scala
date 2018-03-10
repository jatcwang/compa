package com.skedulo.htplay.paths

import cats.Applicative
import cats.data.{EitherT, Kleisli}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converter.ExistConverter
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Query, Request}
import shapeless.ops.hlist.Prepend
import shapeless._
import org.http4s.Method
import scala.language.implicitConversions

import scala.language.higherKinds

sealed trait Segment

// Literal matching of a segment
case class LiteralSegment(str: String) extends Segment
// a path variable may convert to multiple parameters
//TODOO: make you able to pass a name
case class PathVarSegment[Err, A](parser: String => Either[Err, A]) extends Segment

//TODOO: make private
case class PBuilder[F[_], Err, Vars <: HList](
  override val method: Method,
  override val matchSegments: Vector[Segment],
  override val converters: Vector[ExistConverter[Err]]
) extends SuperBuilder[F, Err, Vars] {

  def /(segment: String): PBuilder[F, Err, Vars] =
    this.copy(matchSegments = matchSegments :+ LiteralSegment(segment))

  def /[A](
    segment: PathVarSegment[Err, A]
  )(implicit prepend: Prepend[Vars, A :: HNil]): PBuilder[F, Err, prepend.Out] = {
    val c = StringConverter(segment.parser)
    PBuilder(method, matchSegments :+ segment, converters :+ c)
  }

  def :?[A](
    queryParam: SingleParam[Err, A]
  )(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[F, Err, prepend.Out] =
    QBuilder[F, Err, Vars](method, matchSegments, converters).withQueryParam(queryParam)(prepend)

  override def make(implicit E: HasUriNotMatched[Err], F: Applicative[F]): FFF[F, Request[F], Err, Vars] = {
    val matcher = Matchers.makeMatcher[F, Err, Vars](method, converters, matchSegments)
    Kleisli[EitherT[F, Err, ?], Request[F], Vars] { req =>
      EitherT.fromEither[F](matcher.processReq(req))
    }
  }

  override def prefix(segments: Vector[String]): PBuilder[F, Err, Vars] = {
    this.copy(matchSegments = segments.map(LiteralSegment) ++ matchSegments)
  }
}

object PBuilder {

  val intVar: PathVarSegment[ReqError, Int] = PathVarSegment(
    str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage))
  )
  val stringVar: PathVarSegment[ReqError, String] = PathVarSegment(str => Right(str))

  def makeRoot[F[_], Err] = new Http4sMethodExts[F, Err]

  class Http4sMethodExts[F[_], Err] {
    class Http4sMethodOps(val method: Method) {

      def /(segment: String): PBuilder[F, Err, HNil] =
        PBuilder[F, Err, HNil](method, matchSegments = Vector(LiteralSegment(segment)), converters = Vector.empty)

      def /[A](segment: PathVarSegment[Err, A]): PBuilder[F, Err, A :: HNil] = {
        val c = StringConverter(segment.parser)
        PBuilder[F, Err, A :: HNil](method, matchSegments = Vector(segment), converters = Vector(c))
      }
    }

    implicit def toMethodOps(method: Method) = new Http4sMethodOps(method)
  }


}

sealed trait Converter[In, Err, A] {
  def converter: In => Either[Err, A]
}

object Converter {
  type ExistConverter[Err] = Converter[_, Err, _]
  type AnyConverter[Err]   = Converter[Any, Err, Any]
}

// Convert a string into A
case class StringConverter[Err, A](override val converter: String     => Either[Err, A]) extends Converter[String, Err, A]
case class QueryStringConverter[Err, A](override val converter: Query => Either[Err, A])
    extends Converter[Query, Err, A]
case class RequestConverter[F[_], Err, A](override val converter: Request[F] => Either[Err, A])
    extends Converter[Request[F], Err, A]
