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
}
