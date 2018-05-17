package com.skedulo.htplay.paths

import cats.{Functor, Monad}
import cats.data.{EitherT, Kleisli}
import com.skedulo.htplay.builders.SuperBuilder
import com.skedulo.htplay.paths.Playground.FFF
import com.skedulo.htplay.utils.AsHList
import org.http4s.{Request, Response}
import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Prepend

//TODO: make sealed or package private
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

  def ||(
    fx: FFF[F, Request[F], Err, Unit]
  )(implicit F: Monad[F]): PathPartial[F, Err, Res] = {
    val newPost = (req: Request[F]) => {
      val t: EitherT[F, Err, Unit] = fx.run(req)
      postUriMatchProcessing(req).andThen(vars => t.map(_ => vars))
    }

    new PathPartial[F, Err, Res] {
      override protected type BuilderVars = self.BuilderVars
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self.builder
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Res] = newPost
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
