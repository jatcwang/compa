package com.skedulo.htplay.paths

import cats.Applicative
import cats.data.{EitherT, Kleisli}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converter.ExistConverter
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Method, Request}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil, _}

import scala.language.higherKinds

sealed trait QueryParam[Err, T]
//TODOO: do we need this? we can probably generalize this to use MultiParam (and call headoption)
case class SingleParam[Err, T](key: String, convert: String => Either[Err, T]) extends QueryParam[Err, T]
// indicate that we shouldn't ignore multiple params
case class MultiParam[Err, T](key: String, convert: Seq[String] => Either[Err, T]) extends QueryParam[Err, T]

object QueryParam {
  def int(key: String) = SingleParam[ReqError, Int](key, str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage)))
  def str(key: String) = SingleParam[ReqError, String](key, str => Right(str))
}

//TODOO: make private
case class QBuilder[F[_], Err, Vars <: HList](
  override val method: Method,
  override val matchSegments: Vector[Segment],
  override val converters: Vector[ExistConverter[Err]]
) extends SuperBuilder[F, Err, Vars]{

  def withQueryParam[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[F, Err, prepend.Out] = {
    val converter = QueryStringConverter(q => {
      //TODOO: handle empty
      val paramValue = q.params.get(param.key).get
      param.convert(paramValue)
    })
    this.copy(converters = converters :+ converter)
  }

  def &[A](param: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[F, Err, prepend.Out] = {
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

  override def prefix(segments: Vector[String]): QBuilder[F, Err, Vars] = {
    this.copy(matchSegments = segments.map(LiteralSegment) ++ matchSegments)
  }
}


object QBuilder {

}

