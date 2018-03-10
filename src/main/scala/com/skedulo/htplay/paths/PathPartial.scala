package com.skedulo.htplay.paths

import cats.data.{EitherT, Kleisli}
import cats.{Applicative, Monad}
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.{Request, Response}
import shapeless._
import shapeless.ops.function.{FnFromProduct, FnToProduct}
import shapeless.ops.hlist.Prepend

//TODOO: make sealed or package private
trait PathPartial[F[_], Err, Res <: HList] { self =>
  protected type BuilderVars <: HList
  protected val builder: SuperBuilder[F, Err, BuilderVars]
  protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Res]

  def |[A](fx: FFF[F, Request[F], Err, A])(implicit prepend: Prepend[Res, A :: HNil], F: Monad[F]): PathPartial[F, Err, prepend.Out] = {
    val newPost = (req: Request[F]) => {
      val t: EitherT[F, Err, A] = fx.run(req)
      postUriMatchProcessing(req).andThen(vars => t.map(f => prepend(vars, f :: HNil)))
    }

    new PathPartial[F, Err, prepend.Out] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, prepend.Out] = newPost

    }

  }

  def processCurrent[NewRes](fx: FFF[F, Res, Err, NewRes])(implicit F: Monad[F], asHList: AsHList[NewRes]): PathPartial[F, Err, asHList.Out] = {
    val newPost = (req: Request[F]) => {
      postUriMatchProcessing(req).andThen(vars => fx.run(vars).map(asHList(_)))
    }
    new PathPartial[F, Err, asHList.Out] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, asHList.Out] = newPost
    }
  }

  // Binds it to a function to return a Response
  def |>>(f: Res => F[Response[F]])(implicit F: Monad[F]): PathComplete[F, Err] = {
    new PathComplete[F, Err] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Response[F]] = req => {
        self.postUriMatchProcessing(req).andThen(res => EitherT.liftF[F, Err, Response[F]](f(res)))
      }
    }
  }

  def |>[Func](f: Func)(implicit fnToProduct: FnToProduct.Aux[Func, Res => F[Response[F]]], F: Monad[F]): PathComplete[F, Err] = {
    new PathComplete[F, Err] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Response[F]] = req => {
        self.postUriMatchProcessing(req).andThen(res => EitherT.liftF[F, Err, Response[F]](fnToProduct(f)(res)))
      }
    }
  }

}

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


