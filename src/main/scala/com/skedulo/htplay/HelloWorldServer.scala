package com.skedulo.htplay

import monix.eval.Task
import cats.effect.IO
import fs2.StreamApp
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.impl.EntityResponseGenerator
import org.http4s.rho.bits.ResponseGeneratorInstances
//import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder
import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.rho._
import MonixSched._

object MonixSched {
  import monix.execution.Scheduler.Implicits.global
  implicit val httpScheduler = new monix.eval.instances.CatsEffectForTask
}

object HelloWorldServer extends StreamApp[Task] with ResponseGeneratorInstances[Task] {

//  def world: () => Task[Result.BaseResult[Task]] = { _ =>
//    Ok("asdf")
//  }

  val rhoServ = new RhoService[Task] {
    // A path can be built up in multiple steps and the parts reused
    val pathPart1 = GET / "hello"

    pathPart1 / "world" / pathVar[Int] |>> { (x: Int) => Ok("Hello, world!") }
    pathPart1 / "you" |>> { () => Ok("Hello, you!") }
  }

  val service = rhoServ.toService()

  //  val service: HttpService[Task] = HttpService[IO] {
  //    case GET -> Root / "hello" / name =>
  //      Ok(Json.obj("message" -> Json.fromString(s"Hello, ${name}")))
  //  }

  def stream(args: List[String], requestShutdown: Task[Unit]) =
    BlazeBuilder[Task]
      .bindHttp(8080, "0.0.0.0")
      .mountService(service, "/")
      .serve
}
