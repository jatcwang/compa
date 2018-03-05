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

  type Fx[A, B] = FFF[Task, A, ReqError, B]
  val b: Fx[Request[Task], Int :: String :: HNil] = ???
  val c: Fx[Request[Task], Int] = ???

  val d: Fx[Request[Task], Int :: String :: Int :: Int :: HNil] = b | c | c

  val e: Fx[Int :: String :: HNil, Int] = ???

  val f: Fx[Request[Task], Int] = b |> e

  implicit final class FxExtensions[B <: HList](fx: Fx[Request[Task], B]) {
    // take the request and add more to hlist result |
    def |[C](fx2: Fx[Request[Task], C])(implicit prepend: Prepend[B, C :: HNil]): Fx[Request[Task], prepend.Out] = ???

    // consume the result and produce another
    def |>[C](fx2: Fx[B, C]): Fx[Request[Task], C] = ???
  }

}
