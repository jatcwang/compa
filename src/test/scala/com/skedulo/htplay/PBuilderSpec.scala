package com.skedulo.htplay

import cats.effect.IO
import com.skedulo.htplay.paths.{PBuilder, QBuilder, QueryParam => Q}
import org.http4s._
import org.http4s.implicits._
import org.scalatest.{FreeSpec, Matchers}
import PBuilder._
import monix.eval.Task
import shapeless._

class PBuilderSpec extends FreeSpec with Matchers {

  "PBuilder" - {
    "parses with path variables" in {
      val builder: PBuilder[String, Int :: String :: HNil] = root / "asdf" / intVar / stringVar
      val matcher = builder.make[Task]
      val req = Request[Task](uri = Uri.fromString("https://google.com/asdf/12/world").right.get)
      matcher.processReq(req) shouldEqual Some(12 :: "world" :: HNil)
    }

    "parses path & query param" in {
      val builder: QBuilder[String, Int :: Int :: String :: HNil] = root / "asdf" / intVar :? Q.int("myint") & Q.str("mystr")
      val matcher = builder.make[Task]
      val req = Request[Task](uri = Uri.fromString("https://google.com/asdf/12?myint=5&mystr=hello").right.get)
      matcher.processReq(req) shouldEqual Some(12 :: 5 :: "hello" :: HNil)
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
