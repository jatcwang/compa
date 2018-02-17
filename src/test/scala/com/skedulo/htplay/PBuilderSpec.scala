package com.skedulo.htplay

import cats.effect.IO
import com.skedulo.htplay.paths.PBuilder
import org.http4s._
import org.http4s.implicits._
import org.scalatest.FreeSpec
import PBuilder._
import monix.eval.Task
import shapeless._

class PBuilderSpec extends FreeSpec {

  //TODOO:
  "PBuilder" - {
    "builds string path" in {
      println(root / "asdf" / "asdfa")
    }

    "builds with path variables" in {
      val builder: PBuilder[String, Int :: String :: HNil] = root / "asdf" / intVar / stringVar
      val m: (((Int, String)) => Response[Task]) => Request[Task] => Option[Response[Task]] = builder.toMatcher[Task, (Int, String)]
      val matcher = m((new SomeClass().lol _).tupled)
      val req = Request[Task](uri = Uri.fromString("https://google.com/asdf/12/world").right.get)
      matcher(req)
    }

  }

  class SomeClass() {
    def lol(i: Int, s: String): Response[Task] = {
      println(s"hello $i $s")
      Response.notFound
    }
  }
}
