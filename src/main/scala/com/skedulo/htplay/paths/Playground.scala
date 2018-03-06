package com.skedulo.htplay.paths

import cats.Monad
import cats.data.{EitherT, Kleisli, OptionT}
import org.http4s.{Request, Response, Status}
import cats.implicits._
import monix.eval.Task
import shapeless._
import shapeless.ops.hlist.Prepend

object Playground {

  // A wrapper around A => F[Either[Err, B]
  type FFF[F[_], A, Err, B] = Kleisli[EitherT[F, Err, ?], A, B]

  type ReqFilter[F[_], Err] = FFF[F, Request[F], Err, Response[F]]

  def processRequest[F[_]](filter: ReqFilter[F, ReqError])(implicit F: Monad[F]): Kleisli[OptionT[F, ?], Request[F], Response[F]] = {
    // better convertion to response
    filter.mapF { s: EitherT[F, ReqError, Response[F]] =>
      OptionT.apply(s.value.map {
        case Left(UriNotMatched) => Some(Response.notFound[F])
        case Left(InvalidRequest(d)) => Some(Response(Status.BadRequest))
        case Left(FilterError(e)) => Some(Response(Status.InternalServerError))
        case Right(r) => Some(r)
      })
    }
  }

}
