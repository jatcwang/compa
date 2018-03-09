package com.skedulo.htplay

import cats.data.{EitherT, Kleisli}
import com.skedulo.htplay.paths.PBuilder._
import com.skedulo.htplay.paths.Playground.FFF
import com.skedulo.htplay.paths.{InvalidRequest, PBuilder, QBuilder, ReqError, QueryParam => Q}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.http4s._
import org.http4s.implicits._
import org.scalatest.{AsyncFreeSpec, Matchers}
import shapeless._
import shapeless.ops.function.{FnFromProduct, FnToProduct}

class PBuilderSpec extends AsyncFreeSpec with Matchers {

  val root = rootWith[Task]

  "PBuilder" - {
    "parses with path variables" in {
      val builder: PBuilder[Task, ReqError, Int :: String :: HNil] = root / "asdf" / intVar / stringVar
      val matcher                                                  = builder.make
      val req                                                      = Request[Task](uri = Uri.fromString("https://hello.com/asdf/12/world").right.get)
      matcher
        .run(req)
        .value
        .map { res =>
          res shouldEqual Right(12 :: "world" :: HNil)
        }
        .runAsync
    }

    "parses path & query param" in {
      val builder: QBuilder[Task, ReqError, Int :: Int :: String :: HNil] = root / "asdf" / intVar :? Q.int("myint") & Q
        .str("mystr")
      val matcher = builder.make
      val req     = Request[Task](uri = Uri.fromString("https://hello.com/asdf/12?myint=5&mystr=hello").right.get)
      matcher
        .run(req)
        .value
        .map { res =>
          res shouldEqual Right(12 :: 5 :: "hello" :: HNil)
        }
        .runAsync
    }

    "with request postprocessing pipeline (parsing request)" in {
      val path = root / "asdf" / intVar | auth |>> {
        case i :: user :: HNil =>
          Task.delay {
            assert(i == 12)
            assert(user == User("john"))
            Response(Status.Ok)
          }
      }
      val req = Request[Task](
        uri     = Uri.fromString("https://hello.com/asdf/12?myint=5&mystr=hello").right.get,
        headers = Headers(Header("Authorization", "john"))
      )
      path.make
        .run(req)
        .value
        .map { res =>
          res shouldEqual Right(Response(Status.Ok))
        }
        .runAsync
    }

    "with request postprocessing pipeline (depending on previous results)" in {
      val path = (root / "asdf" / intVar / intVar processCurrent makeCoordinate) |>> { rect =>
        Task.delay {
          assert(rect == Rectangle(2, 3) :: HNil)
          Response(Status.Ok)
        }
      }
      val req = Request[Task](uri = Uri.fromString("https://hello.com/asdf/2/3").right.get)
      path.make
        .run(req)
        .value
        .map { res =>
          res shouldEqual Right(Response(Status.Ok))
        }
        .runAsync
    }

    "sugar syntax for binding to request handlers defined in the form of functions" in {
      val path = root / "asdf" / intVar | auth |> (myRequestHandler _)
      val req = Request[Task](
        uri     = Uri.fromString("https://hello.com/asdf/12?myint=5&mystr=hello").right.get,
        headers = Headers(Header("Authorization", "john"))
      )
      path.make
        .run(req)
        .value
        .map { res =>
          res shouldEqual Right(Response(Status.Ok))
        }
        .runAsync
    }

    def auth: FFF[Task, Request[Task], ReqError, User] = {
      Kleisli[EitherT[Task, ReqError, ?], Request[Task], User] { req =>
        EitherT.fromOption[Task](req.headers.get("Authorization".ci).map { authHeaderValue =>
          User(authHeaderValue.value)
        }, InvalidRequest("not authorized"))

      }
    }

    def makeCoordinate: FFF[Task, Int :: Int :: HNil, ReqError, Rectangle] =
      Kleisli[EitherT[Task, ReqError, ?], Int :: Int :: HNil, Rectangle] {
        case (w :: h :: HNil) =>
          EitherT.pure(Rectangle(w, h))
      }

  }

  case class User(name: String)
  case class Rectangle(width: Int, height: Int)

  def myRequestHandler(id: Int, user: User): Task[Response[Task]] =
    Task.now(Response(status = Status.Ok))

  def anotherHandler(id: Int, f: Int, t: Int, user: User): Task[Response[Task]] =
    Task.now(Response(status = Status.Ok))

}
/*
//TODOO:
empty
?a=&b=

multi param, possibly empty
?a=&a=
 */
