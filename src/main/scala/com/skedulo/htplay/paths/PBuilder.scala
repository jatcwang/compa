package com.skedulo.htplay.paths

import cats.Applicative
import cats.data.{EitherT, Kleisli}
import org.http4s.{Query, Request, Response}
import shapeless.{HList, HNil}
import shapeless._
import shapeless.ops.hlist.{Prepend, Tupler}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converter.{AnyConverter, ExistConverter}
import com.skedulo.htplay.paths.Playground.FFF
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

// Can prepend some paths
trait PathPrependable[Builder] {
  def prefixPaths(builder: Builder, paths: Vector[String]): Builder
}

trait SuperBuilder[Err, Vars <: HList] {

  def make[F[_]: Applicative](implicit trav: FromTraversable[Vars], E: HasUriNotMatched[Err]): FFF[F, Request[F], Err, Vars]
}

//TODOO: make private
case class PBuilder[Err, Vars <: HList](matchSegments: Vector[Segment], converters: Vector[ExistConverter[Err]]) extends SuperBuilder[Err, Vars]{

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

  def prepend(paths: Vector[String]): PBuilder[Err, Vars] = {
    this.copy(paths.map(LiteralSegment) ++ matchSegments, converters)
  }

  override def make[F[_]: Applicative](implicit trav: FromTraversable[Vars], E: HasUriNotMatched[Err]): FFF[F, Request[F], Err, Vars] = {
    val matcher = Matchers.makeMatcher[F, Err, Vars](converters, matchSegments)
    Kleisli[EitherT[F, Err, ?], Request[F], Vars]{ req =>
      EitherT.fromEither[F](matcher.processReq(req))
    }
  }
}

object PBuilder {
  val root: PBuilder[ReqError, HNil] = PBuilder(
    matchSegments = Vector.empty,
    converters = Vector.empty
  )

  val intVar: PathVarSegment[ReqError, Int] = PathVarSegment(str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage)))
  val stringVar: PathVarSegment[ReqError, String] = PathVarSegment(str => Right(str))

  implicit def pathPrependable[Err, Vars <: HList]: PathPrependable[PBuilder[Err, Vars]] = new PathPrependable[PBuilder[Err, Vars]] {
    override def prefixPaths(builder: PBuilder[Err, Vars], paths: Vector[String]): PBuilder[Err, Vars] = builder.prepend(paths)
  }
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
