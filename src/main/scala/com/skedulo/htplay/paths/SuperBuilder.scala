package com.skedulo.htplay.paths

import cats.data.{EitherT, Kleisli}
import cats.{Applicative, Monad}
import com.skedulo.htplay.paths.Converter.ExistConverter
import com.skedulo.htplay.paths.Playground.FFF
import org.http4s.Request
import shapeless.{HNil, _}
import shapeless.ops.hlist.Prepend

trait SuperBuilder[F[_], Err, Res] { self =>
  protected def matchSegments: Vector[Segment]
  protected def converters: Vector[ExistConverter[Err]]

  def make(implicit E: HasUriNotMatched[Err], F: Applicative[F]): FFF[F, Request[F], Err, Res]

  def |[A, ResAsHList <: HList](
    fx: FFF[F, Request[F], Err, A]
  )(implicit asHList: AsHList.Aux[Res, ResAsHList], prepend: Prepend[ResAsHList, A :: HNil], F: Monad[F]): PathPartial[F, Err, prepend.Out] = {
    val newPost = (req: Request[F]) => {
      Kleisli[EitherT[F, Err, ?], Res, prepend.Out] { res =>
        val t: EitherT[F, Err, A] = fx.run(req)
        t.map(f => prepend(asHList(res), f :: HNil))
      }
    }

    new PathPartial[F, Err, prepend.Out] {
      override protected type BuilderVars = Res
      override protected val builder: SuperBuilder[F, Err, BuilderVars] = self
      override protected val postUriMatchProcessing: Request[F] => FFF[F, BuilderVars, Err, prepend.Out] = newPost
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


}


