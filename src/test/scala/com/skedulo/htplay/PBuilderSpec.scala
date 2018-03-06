package com.skedulo.htplay

import cats.effect.IO
import com.skedulo.htplay.paths.{InvalidRequest, PBuilder, QBuilder, ReqError, QueryParam => Q}
import org.http4s._
import org.http4s.implicits._
import org.scalatest.{AsyncFreeSpec, Matchers}
import PBuilder._
import cats.data.{EitherT, Kleisli}
import com.skedulo.htplay.paths.Playground.FFF
import monix.eval.Task
import shapeless._
import shapeless.ops.traversable._
import monix.execution.Scheduler.Implicits.global
import org.http4s.headers.Authorization

class PBuilderSpec extends AsyncFreeSpec with Matchers {

  val root = rootWith[Task]

  "PBuilder" - {
    "parses with path variables" in {
      val builder: PBuilder[Task, ReqError, Int :: String :: HNil] = root / "asdf" / intVar / stringVar
      val matcher = builder.make
      val req = Request[Task](uri = Uri.fromString("https://hello.com/asdf/12/world").right.get)
      matcher.run(req).value.map { res =>
        res shouldEqual Right(12 :: "world" :: HNil)
      }.runAsync
    }

    "parses path & query param" in {
      val builder: QBuilder[Task, ReqError, Int :: Int :: String :: HNil] = root / "asdf" / intVar :? Q.int("myint") & Q.str("mystr")
      val matcher = builder.make
      val req = Request[Task](uri = Uri.fromString("https://hello.com/asdf/12?myint=5&mystr=hello").right.get)
      matcher.run(req).value.map { res =>
        res shouldEqual Right(12 :: 5 :: "hello" :: HNil)
      }.runAsync
    }

    "with request postprocessing pipeline (parsing request)" in {
      val path = root / "asdf" / intVar | auth |>> { case i :: user :: HNil =>
          Task.delay {
            assert(i == 12)
            assert(user == User("john"))
            Response(Status.Ok)
          }
      }
      val req = Request[Task](
        uri = Uri.fromString("https://hello.com/asdf/12?myint=5&mystr=hello").right.get,
        headers = Headers(Header("Authorization", "john"))
      )
      path.make.run(req).value.map { res =>
        res shouldEqual Right(Response(Status.Ok))
      }.runAsync
    }

    "with request postprocessing pipeline (depending on previous results)" in {
//      val path = root / "asdf" / intVar / intVar |> ()
      pending
    }

    def auth: FFF[Task, Request[Task], ReqError, User] = {
      Kleisli[EitherT[Task, ReqError, ?], Request[Task], User] { req =>
        EitherT.fromOption[Task](
          req.headers.get("Authorization".ci).map { authHeaderValue =>
            User(authHeaderValue.value)
          },
          InvalidRequest("not authorized")
        )

      }
    }

  }

  case class User(name: String)

}


/*
//TODOO:
empty
?a=&b=

multi param, possibly empty
?a=&a=
 */
