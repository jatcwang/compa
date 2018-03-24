package com.skedulo.htplay.builders

import cats.Applicative
import cats.data.{EitherT, Kleisli}
import cats.syntax.either._
import com.skedulo.htplay.builders
import com.skedulo.htplay.builders.Converter.ExistConverter
import com.skedulo.htplay.builders.QueryParam.SingleParam
import com.skedulo.htplay.simple.{InvalidRequest, ReqError}
import com.skedulo.htplay.paths._
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Method, Request}
import shapeless._
import shapeless.ops.hlist.Prepend

import scala.language.{higherKinds, implicitConversions}

//TODOO: deal with encoding/decoding issues in specified paths
case class PathBuilder[F[_], Err, Vars <: HList] private (
  override val method: Method,
  override val matchSegments: Vector[PathSegment],
  override val converters: Vector[ExistConverter[Err]]
) extends SuperBuilder[F, Err, Vars] {

  def /(segment: String): PathBuilder[F, Err, Vars] =
    this.copy(matchSegments = matchSegments :+ LiteralSegment(segment))

  def /[A](
    segment: PathVarSegment[Err, A]
  )(implicit prepend: Prepend[Vars, A :: HNil]): PathBuilder[F, Err, prepend.Out] = {
    val c = StringConverter(segment.parser)
    builders.PathBuilder(method, matchSegments :+ segment, converters :+ c)
  }

  def :?[A](
    queryParam: SingleParam[Err, A]
  )(implicit prepend: Prepend[Vars, A :: HNil]): QueryBuilder[F, Err, prepend.Out] =
    QueryBuilder[F, Err, Vars](method, matchSegments, converters).withQueryParam(queryParam)(prepend)

  override def make(implicit E: HasUriNotMatched[Err], F: Applicative[F]): FFF[F, Request[F], Err, Vars] = {
    val matcher = Matchers.makeMatcher[F, Err, Vars](method, converters, matchSegments)
    Kleisli[EitherT[F, Err, ?], Request[F], Vars] { req =>
      EitherT.fromEither[F](matcher.processReq(req))
    }
  }

  override def prefix(segments: Vector[String]): PathBuilder[F, Err, Vars] = {
    this.copy(matchSegments = segments.map(LiteralSegment) ++ matchSegments)
  }
}

object PathBuilder {

  val intVar: PathVarSegment[ReqError, Int] = PathVarSegment(
    str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage))
  )
  val stringVar: PathVarSegment[ReqError, String] = PathVarSegment(str => Right(str))

  def makeRoot[F[_], Err] = new Http4sMethodExts[F, Err]

  class Http4sMethodExts[F[_], Err] {
    class Http4sMethodOps(val method: Method) {

      def /(segment: String): PathBuilder[F, Err, HNil] =
        PathBuilder[F, Err, HNil](method, matchSegments = Vector(LiteralSegment(segment)), converters = Vector.empty)

      def /[A](segment: PathVarSegment[Err, A]): PathBuilder[F, Err, A :: HNil] = {
        val c = StringConverter(segment.parser)
        builders.PathBuilder[F, Err, A :: HNil](method, matchSegments = Vector(segment), converters = Vector(c))
      }
    }

    implicit def toMethodOps(method: Method) = new Http4sMethodOps(method)
  }


}

