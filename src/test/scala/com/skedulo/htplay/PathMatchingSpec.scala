package com.skedulo.htplay

import com.skedulo.htplay.paths._
import com.skedulo.htplay.route.RouteGroup
import monix.eval.Task
import org.http4s._
import org.scalatest.{AsyncFreeSpec, Matchers}
import monix.execution.Scheduler.Implicits.global
import org.http4s.Method.{GET, DELETE}

class PathMatchingSpec extends AsyncFreeSpec with Matchers {

  val setup = PathBuilder.makeRoot[Task, ReqError]
  import setup._

  val UNAUTHORIZED = Response[Task](Status.Unauthorized)
  val OK = Response[Task](Status.Ok)
  val CONFLICT = Response[Task](Status.Conflict)

  //TODO: document deprecation warning for Function0 using eta expansion syntax
  "group of paths matches successfully" in {
    val group = RouteGroup(
      GET / "rejected" |> (() => return401),
      GET / "ok"       |> (() => return200),
      DELETE / "conflict" |> (() => return409)
    )

    val go = group.toHttpService(convertReqError).run

    (for {
      r401 <- go(Request(uri = makeUri("rejected"))).value
      r200 <- go(Request(uri = makeUri("ok"))).value
      r409 <- go(Request(method = DELETE, uri = makeUri("conflict"))).value
      rNotFound <- go(Request(uri = makeUri("invalid/path"))).value
    } yield {
      r401 shouldEqual Some(UNAUTHORIZED)
      r200 shouldEqual Some(OK)
      r409 shouldEqual Some(CONFLICT)
      rNotFound shouldEqual None
    }).runAsync
  }
  //TODOO: check trailing slash

  def return401: Task[Response[Task]] = {
    Task.now(UNAUTHORIZED)
  }

  def return200: Task[Response[Task]] = {
    Task.now(OK)
  }

  def return409: Task[Response[Task]] = {
    Task.now(CONFLICT)
  }

  def convertReqError(err: ReqError): Task[Option[Response[Task]]] = {
    err match {
      case UriNotMatched => Task.now(None)
      case InvalidRequest(msg) => {
        Response[Task](Status.BadRequest).withBody(msg).map(Some(_))
      }
      case FilterError(e) => Task.now(Some(Response(Status.InternalServerError)))
    }
  }

  def makeUri(str: String): Uri = {
    Uri.fromString("https://base.come/" + str).right.get
  }
}
