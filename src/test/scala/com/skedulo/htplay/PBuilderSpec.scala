package com.skedulo.htplay

import cats.effect.IO
import com.skedulo.htplay.paths.{PBuilder, QBuilder, ReqError, QueryParam => Q}
import org.http4s._
import org.http4s.implicits._
import org.scalatest.{AsyncFreeSpec, Matchers}
import PBuilder._
import monix.eval.Task
import shapeless._
import monix.execution.Scheduler.Implicits.global

class PBuilderSpec extends AsyncFreeSpec with Matchers {

  val root = rootWith[Task]

  "PBuilder" - {
    "parses with path variables" in {
      val builder: PBuilder[Task, ReqError, Int :: String :: HNil] = root / "asdf" / intVar / stringVar
      val matcher = builder.make
      val req = Request[Task](uri = Uri.fromString("https://google.com/asdf/12/world").right.get)
      matcher.run(req).value.map { res =>
        res shouldEqual Right(12 :: "world" :: HNil)
      }.runAsync
    }

    "parses path & query param" in {
      val builder: QBuilder[Task, ReqError, Int :: Int :: String :: HNil] = root / "asdf" / intVar :? Q.int("myint") & Q.str("mystr")
      val matcher = builder.make
      val req = Request[Task](uri = Uri.fromString("https://google.com/asdf/12?myint=5&mystr=hello").right.get)
      matcher.run(req).value.map { res =>
        res shouldEqual Right(12 :: 5 :: "hello" :: HNil)
      }.runAsync
    }

  }

  class SomeClass() {
    def lol(i: Int, s: String): Response[Task] = {
      println(s"hello $i $s")
      Response.notFound
    }
  }
}


/*
empty
?a=&b=

multi param, possibly empty
?a=&a=
 */
