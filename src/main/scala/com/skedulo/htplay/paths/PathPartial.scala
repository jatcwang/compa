package com.skedulo.htplay.paths

import cats.data.{EitherT, Kleisli}
import cats.{Applicative, Monad}
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Request, Response}
import shapeless._
import shapeless.ops.hlist.Prepend

//TODOO: make sealed or package private
trait PathPartial[F[_], Err, Res] { p =>
  protected type BuilderVars
  protected val builder: SuperBuilder[F, Err, BuilderVars]
  protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Res]

  def |[A, ThisRes <: HList](fx: FFF[F, Request[F], Err, A])(implicit prepend: Prepend[ThisRes, A :: HNil], F: Monad[F], equiv: Res =:= ThisRes): PathPartial[F, Err, prepend.Out] = {
    val newPost = (req: Request[F]) => {
      val t: EitherT[F, Err, A] = fx.run(req)
      postUriMatchProcessing(req).andThen(vars => t.map(f => prepend(equiv(vars), f :: HNil)))
    }

    new PathPartial[F, Err, prepend.Out] {
      override protected type BuilderVars = p.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = p.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, prepend.Out] = newPost

    }

  }

  def |>[NewRes](fx: FFF[F, Res, Err, NewRes])(implicit F: Monad[F]): PathPartial[F, Err, NewRes] = {
    val newPost = (req: Request[F]) => {
      postUriMatchProcessing(req).andThen(vars => fx.run(vars))
    }
    new PathPartial[F, Err, NewRes] {
      override protected type BuilderVars = p.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = p.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, NewRes] = newPost
    }
  }

  // Binds it to a function to return a Response
  def |>>(f: Res => F[Response[F]])(implicit F: Monad[F]): PathComplete[F, Err] = {
    new PathComplete[F, Err] {
      override protected type BuilderVars = p.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = p.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Response[F]] = req => {
        p.postUriMatchProcessing(req).andThen(res => EitherT.liftF[F, Err, Response[F]](f(res)))
      }
    }
  }

}

sealed trait PathComplete[F[_], Err] {
  protected type BuilderVars
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
}


