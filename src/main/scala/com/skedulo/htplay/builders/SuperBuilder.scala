package com.skedulo.htplay.builders

import cats.data.{EitherT, Kleisli}
import cats.{Applicative, Functor, Monad}
import com.skedulo.htplay.builders.Converter.ExistConverter
import com.skedulo.htplay.paths.Playground.FFF
import com.skedulo.htplay.paths._
import com.skedulo.htplay.utils.AsHList
import org.http4s.{Method, Request, Response}
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Prepend
import shapeless._

trait SuperBuilder[F[_], Err, Res <: HList] { self =>
  protected def method: Method
  protected def matchSegments: Vector[PathSegment]
  protected def converters: Vector[ExistConverter[Err]]

  def make(implicit E: HasUriNotMatched[Err], F: Applicative[F]): FFF[F, Request[F], Err, Res]

  def |[A](
    fx: FFF[F, Request[F], Err, A]
  )(implicit prepend: Prepend[Res, A :: HNil], F: Monad[F]): PathPartial[F, Err, prepend.Out] = {
    val newPost = (req: Request[F]) => {
      Kleisli[EitherT[F, Err, ?], Res, prepend.Out] { res =>
        val t: EitherT[F, Err, A] = fx.run(req)
        t.map(f => prepend(res, f :: HNil))
      }
    }

    new PathPartial[F, Err, prepend.Out] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, prepend.Out] = newPost
    }
  }

  def ||(
    fx: FFF[F, Request[F], Err, Unit]
  )(implicit F: Functor[F]): PathPartial[F, Err, Res] = {
    val newPost = (req: Request[F]) => {
      Kleisli[EitherT[F, Err, ?], Res, Res] { res =>
        val t: EitherT[F, Err, Unit] = fx.run(req)
        t.map(_ => res)
      }
    }

    new PathPartial[F, Err, Res] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, Res] = newPost
    }
  }

  //TODOO: need to improve UX when Res is HList
  def processCurrent[NewRes](fx: FFF[F, Res, Err, NewRes])(implicit F: Monad[F], asHList: AsHList[NewRes]): PathPartial[F, Err, asHList.Out] = {
    new PathPartial[F, Err, asHList.Out] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, asHList.Out] = _ => fx.map(asHList(_))
    }
  }

  //TODOO: take validated inputs (e.g. can't contain "/" in each string)
  def prefix(segments: Vector[String]): SuperBuilder[F, Err, Res]

  // Binds it to a function to return a Response
  def |>>(f: Res => F[Response[F]])(implicit F: Monad[F]): PathComplete[F, Err] = {
    new PathComplete[F, Err] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, Res] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, Res, Err, Response[F]] = req => {
        Kleisli[EitherT[F, Err, ?], Res, Response[F]](res => EitherT.liftF[F, Err, Response[F]](f(res)))
      }
    }
  }

  def |>[Func](f: Func)(implicit fnToProduct: FnToProduct.Aux[Func, Res => F[Response[F]]], F: Monad[F]): PathComplete[F, Err] = {
    val hlistFunc = fnToProduct(f)
    new PathComplete[F, Err] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, Res] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, Res, Err, Response[F]] = req => {
        Kleisli[EitherT[F, Err, ?], Res, Response[F]](res => {
          EitherT.liftF[F, Err, Response[F]](hlistFunc(res))
        })
      }
    }
  }

}


