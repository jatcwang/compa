package com.skedulo.htplay.builders

import cats.Applicative
import cats.data.{EitherT, Kleisli}
import cats.syntax.either._
import com.skedulo.htplay.builders.Converter.ExistConverter
import com.skedulo.htplay.easy.{InvalidRequest, ReqError}
import com.skedulo.htplay.paths.Playground.FFF
import com.skedulo.htplay.paths.{HasUriNotMatched, LiteralSegment, Matchers, PathSegment}
import org.http4s.{Method, Request}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil, _}

import scala.language.higherKinds

case class QueryBuilder[F[_], Err, Vars <: HList] private (
  override val method: Method,
  override val matchSegments: Vector[PathSegment],
  override val converters: Vector[ExistConverter[Err]]
) extends SuperBuilder[F, Err, Vars]{

  def withQueryParam[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QueryBuilder[F, Err, prepend.Out] = {
    val converter = QueryStringConverter(q => {
      //TODOO: handle empty
      val paramValue = q.params.get(param.key).get
      param.convert(paramValue)
    })
    this.copy(converters = converters :+ converter)
  }

  def &[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QueryBuilder[F, Err, prepend.Out] = {
    // pass in the same implicit so compiler can prove that the return type
    // is the same (due to path-dependent types)
    withQueryParam(param)(prepend)
  }

  override def make(implicit E: HasUriNotMatched[Err], F: Applicative[F]): FFF[F, Request[F], Err, Vars] = {
    val matcher = Matchers.makeMatcher[F, Err, Vars](method, converters, matchSegments)
    Kleisli[EitherT[F, Err, ?], Request[F], Vars]{ req =>
      EitherT.fromEither[F](matcher.processReq(req))
    }
  }

  override def prefix(segments: Vector[String]): QueryBuilder[F, Err, Vars] = {
    this.copy(matchSegments = segments.map(LiteralSegment) ++ matchSegments)
  }
}


