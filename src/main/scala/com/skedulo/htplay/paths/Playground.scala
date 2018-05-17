package com.skedulo.htplay.paths

import cats.data.{EitherT, Kleisli}
import org.http4s.{Request, Response}

object Playground {

  // A wrapper around A => F[Either[Err, B]
  type FFF[F[_], A, Err, B] = Kleisli[EitherT[F, Err, ?], A, B]

  type ReqFilter[F[_], Err] = FFF[F, Request[F], Err, Response[F]]
}
