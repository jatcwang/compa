package com.skedulo.htplay

import cats.data.{EitherT, Kleisli}
import com.skedulo.htplay.builders.{PathBuilder, QueryBuilder}
import com.skedulo.htplay.easy.{InvalidRequest, ReqError}
import com.skedulo.htplay.builders.PathBuilder._
import com.skedulo.htplay.paths.Playground.FFF
import com.skedulo.htplay.builders.{QueryParam => Q}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.http4s._
import org.http4s.implicits._
import org.scalatest.{AsyncFreeSpec, Inside, Matchers}
import shapeless._
import org.http4s.Method.GET

class PBuilderSpec extends AsyncFreeSpec with Matchers with Inside {

  val setup = makeRoot[Task, ReqError]
  import setup._

  "Basic Builders" - {
    "parses with path variables" in {
      val builder: PathBuilder[Task, ReqError, Int :: String :: HNil] = GET / "asdf" / intVar / stringVar
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
      val builder: QueryBuilder[Task, ReqError, Int :: Int :: String :: HNil] = GET / "asdf" / intVar :? Q.int("myint") & Q
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
      val path = GET / "asdf" / intVar | auth |>> {
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
      val path = (GET / "asdf" / intVar / intVar processCurrent makeCoordinate) |>> { rect =>
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

    "sugar syntax for binding to request handlers that are functions" in {
      val path = GET / "asdf" / intVar | auth |> (myRequestHandler _)
      val req = Request[Task](
        uri     = Uri.fromString("https://hello.com/asdf/12").right.get,
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

    "postprocess with filter (filter produces no output)" in {
      val path = GET / "asdf" / intVar || authFilter |> (justIntHandler _)
      def makeReq(authHeader: String) = Request[Task](
        uri     = Uri.fromString("https://hello.com/asdf/12").right.get,
        headers = Headers(Header("Authorization", authHeader))
      )

      val validReq = makeReq("john")
      val invalidReq = makeReq("nope")

      val matcher = path.make

      Task.map2(matcher.run(validReq).value, matcher.run(invalidReq).value) { case (validResp, invalidResp) =>
        validResp shouldEqual Right(Response(Status.Ok))
        invalidResp shouldEqual Left(InvalidRequest("not authorized"))
      }.runAsync

    }

  }

  "PathPartial" - {

    "postprocess with filter (filter produces no output)" in {
      // alwaySucceedFilter turns this into a PathPartial
      val path = GET / "asdf" / intVar || alwaySucceedFilter || authFilter |> (justIntHandler _)
      def makeReq(authHeader: String) = Request[Task](
        uri     = Uri.fromString("https://hello.com/asdf/12").right.get,
        headers = Headers(Header("Authorization", authHeader))
      )

      val validReq = makeReq("john")
      val invalidReq = makeReq("nope")

      val matcher = path.make

      Task.map2(matcher.run(validReq).value, matcher.run(invalidReq).value) { case (validResp, invalidResp) =>
        validResp shouldEqual Right(Response(Status.Ok))
        invalidResp shouldEqual Left(InvalidRequest("not authorized"))
      }.runAsync

    }
  }

  case class User(name: String)
  case class Rectangle(width: Int, height: Int)

  def justIntHandler(id: Int): Task[Response[Task]] =
    Task.now(Response(status = Status.Ok))

  def myRequestHandler(id: Int, user: User): Task[Response[Task]] =
    Task.now(Response(status = Status.Ok))

  def anotherHandler(id: Int, f: Int, t: Int, user: User): Task[Response[Task]] =
    Task.now(Response(status = Status.Ok))

  // Filters

  def auth: FFF[Task, Request[Task], ReqError, User] = {
    Kleisli[EitherT[Task, ReqError, ?], Request[Task], User] { req =>
      EitherT.fromOption[Task](req.headers.get("Authorization".ci).map { authHeaderValue =>
        User(authHeaderValue.value)
      }, InvalidRequest("not authorized"))
    }
  }

  def authFilter: FFF[Task, Request[Task], ReqError, Unit] = {
    Kleisli[EitherT[Task, ReqError, ?], Request[Task], Unit] { req =>
      EitherT.fromOption[Task](req.headers.get("Authorization".ci).filter(s => s.value == "john").map(_ => ()), InvalidRequest("not authorized"))
    }
  }

  def alwaySucceedFilter: FFF[Task, Request[Task], ReqError, Unit] = {
    Kleisli[EitherT[Task, ReqError, ?], Request[Task], Unit] { _ =>
      EitherT.pure[Task, ReqError](())
    }
  }

  def makeCoordinate: FFF[Task, Int :: Int :: HNil, ReqError, Rectangle] =
    Kleisli[EitherT[Task, ReqError, ?], Int :: Int :: HNil, Rectangle] {
      case (w :: h :: HNil) =>
        EitherT.pure(Rectangle(w, h))
    }


}
/*
//TODOO:
empty
?a=&b=

multi param, possibly empty
?a=&a=
 */
