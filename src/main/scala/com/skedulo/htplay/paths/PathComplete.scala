package com.skedulo.htplay.paths

import cats.Monad
import cats.data.{EitherT, Kleisli}
import com.skedulo.htplay.builders.SuperBuilder
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Request, Response}
import shapeless._

trait PathComplete[F[_], Err] { self =>
  protected type BuilderVars <: HList
  protected val builder: SuperBuilder[F, Err, BuilderVars]
  protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Response[F]]

  final def make()(implicit F: Monad[F], E: HasUriNotMatched[Err]): FFF[F, Request[F], Err, Response[F]] = {
    val builderMatch: FFF[F, Request[F], Err, BuilderVars] = builder.make
    Kleisli[EitherT[F, Err, ?], Request[F], Response[F]] { req =>
      val urlMatchingStage: EitherT[F, Err, BuilderVars] = builderMatch.run(req)
      val postMatchingStage = postUriMatchProcessing(req)
      urlMatchingStage.flatMap(builderVars => postMatchingStage.run(builderVars))
    }
  }

  def prefix(segments: Vector[String]): PathComplete[F, Err] = {
    new PathComplete[F, Err] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, self.BuilderVars] = builder.prefix(segments)
      override protected val postUriMatchProcessing: Request[F] => FFF[F, this.BuilderVars, Err, Response[F]] = self.postUriMatchProcessing
    }
  }
}



