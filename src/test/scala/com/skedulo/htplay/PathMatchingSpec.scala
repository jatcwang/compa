package com.skedulo.htplay

import com.skedulo.htplay.route.RouteGroup
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.http4s.Method.{DELETE, GET}
import org.http4s._
import org.scalatest.{AsyncFreeSpec, Matchers}

class PathMatchingSpec extends AsyncFreeSpec with Matchers with SimpleRouteSpecification {

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

    val go = group.toHttpService(this.errorToResponse).run

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
  //TODO: check trailing slash

  def return401: Task[Response[Task]] = {
    Task.now(UNAUTHORIZED)
  }

  def return200: Task[Response[Task]] = {
    Task.now(OK)
  }

  def return409: Task[Response[Task]] = {
    Task.now(CONFLICT)
  }

  def makeUri(str: String): Uri = {
    Uri.fromString("https://base.come/" + str).right.get
  }
}
