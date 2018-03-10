package com.skedulo.htplay.route

import cats.Monad
import cats.data.{EitherT, Kleisli, NonEmptyList, OptionT}
import cats.syntax.flatMap._
import com.skedulo.htplay.paths.{HasUriNotMatched, PathComplete}
import org.http4s.{HttpService, Request, Response}

// A group of routes
class RouteGroup[F[_], Err](paths: NonEmptyList[PathComplete[F, Err]]) {
  def prefix(segments: Vector[String]): RouteGroup[F, Err] =
    new RouteGroup(paths.map(_.prefix(segments)))

  // TODO: currently uses matching every path until a result is found,
  // but we have information to allow for much smarter/efficient matching using a binary tree approach
  def toHttpService(
    convError: Err => F[Option[Response[F]]]
  )(implicit F: Monad[F], E: HasUriNotMatched[Err]): HttpService[F] = {
    val matchers = paths.map(_.make.run).toList
    Kleisli[OptionT[F, ?], Request[F], Response[F]] { req =>
      OptionT(doMatch(matchers, convError, req))
    }
  }

  private def doMatch(
    matchers: List[Request[F] => EitherT[F, Err, Response[F]]],
    convError: Err => F[Option[Response[F]]],
    req: Request[F]
  )(implicit F: Monad[F]): F[Option[Response[F]]] = {
    matchers match {
      case List()      => F.pure[Option[Response[F]]](None)
      case m :: others => {
        val eitherT: EitherT[F, Err, Response[F]] = m(req)
        eitherT.value.flatMap {
          case Left(err) => convError(err).flatMap[Option[Response[F]]] {
            case s @ Some(resp) => F.pure[Option[Response[F]]](s)
            case None => doMatch(others, convError, req)
          }
          case Right(response) => F.pure[Option[Response[F]]](Some(response))
        }
      }
    }
  }
}

object RouteGroup {
  def apply[F[_], Err](path: PathComplete[F, Err], others: PathComplete[F, Err]*): RouteGroup[F, Err] =
    new RouteGroup[F, Err](NonEmptyList(path, others.toList))
}
