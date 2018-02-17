package com.skedulo.htplay.paths

import org.http4s.{Query, Request, Response}
import shapeless.{HList, HNil}
import shapeless._
import shapeless.ops.hlist.{Prepend, Tupler}
import cats.syntax.either._
import com.skedulo.htplay.paths.Converters.{AnyConverter, ExistConverter}
import shapeless.ops.traversable.FromTraversable

import scala.collection.{mutable => mut}
import shapeless.syntax.std.traversable._

import scala.language.higherKinds

sealed trait Segment

// Literal matching of a segment
case class LiteralSegment(str: String) extends Segment
// a path variable may convert to multiple parameters
//TODOO: make you able to pass a name
case class PathVarSegment[Err, A](parser: String => Either[Err, A]) extends Segment

//TODOO: make private
case class PBuilder[Err, Vars <: HList](matchSegments: Vector[Segment], converters: Vector[ExistConverter[Err]]) {

  def /(segment: String): PBuilder[Err, Vars] = {
    PBuilder[Err, Vars](matchSegments :+ LiteralSegment(segment), converters)
  }

  def /[A](segment: PathVarSegment[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): PBuilder[Err, prepend.Out] = {
    val c = StringConverter(segment.parser)
    PBuilder(matchSegments :+ segment, converters :+ c)
  }

  def :?[A](queryParam: SingleParam[Err, A])(implicit prepend: Prepend[Vars, A :: HNil]): QBuilder[Err, prepend.Out] = {
    this.toQBuilder.withQueryParam(queryParam)(prepend)
  }

  private def toQBuilder: QBuilder[Err, Vars] = QBuilder(matchSegments, converters)

  def toMatcher[F[_], Tup](implicit toTuple: Generic.Aux[Tup, Vars], fromTraversable: FromTraversable[Vars]): (Tup => Response[F]) => Request[F] => Option[Response[F]] = {
    (cb: Tup => Response[F]) => (req: Request[F]) => {
      val pathSegments = req.uri.path.split('/')
        .filter(_.nonEmpty)
      if (pathSegments.length == matchSegments.length) {
        var idx = 0
        var cont = true
        val accum = mut.ArrayBuffer.empty[Any]
        while (idx < pathSegments.length) {
          val thisSeg = pathSegments(idx)
          matchSegments(idx) match {
            case LiteralSegment(str) => if (str != thisSeg) {
              cont = false
            }
            case p: PathVarSegment[err, a] => {
              accum += thisSeg
            }
          }
          idx += 1
        }
        if (cont) {
          // verified at least all literal segments have been matched
          val results = accum.zip(this.converters).map { case (value, conv) =>
            conv.asInstanceOf[AnyConverter[Err]].converter(value).right.get
          }
          val parseResult = toTuple.from(results.toHList[Vars].get)
          Some(cb(parseResult))
        }
        else None
      }
      else {
        None
      }
    }
  }
}

object PBuilder {
  val root: PBuilder[String, HNil] = PBuilder(
    matchSegments = Vector.empty,
    converters = Vector.empty
  )

  val intVar: PathVarSegment[String, Int] = PathVarSegment(str => Either.catchNonFatal(str.toInt).leftMap(_.getMessage))
  val stringVar: PathVarSegment[String, String] = PathVarSegment(str => Right(str))
}

sealed trait Converters[In, Err, A] {
  def converter: In => Either[Err, A]
}

object Converters {
  type ExistConverter[Err] = Converters[_, Err, _]
  type AnyConverter[Err] = Converters[Any, Err, Any]
}

// Convert a string into A
case class StringConverter[Err, A](override val converter: String => Either[Err, A]) extends Converters[String, Err, A]
case class QueryStringConverter[Err, A](override val converter: Query => Either[Err, A]) extends Converters[Query, Err, A]
case class RequestConverter[F[_], Err, A](override val converter: Request[F] => Either[Err, A]) extends Converters[Request[F], Err, A]

/*
root / 'lol / 'hello

  */

/*
Matching:
  count segments, filter out any RequestMatcher that doesn't have the right number of segments

Converters:
  hlist of converters
  converters has two types
  StringConverter: String => Either[Err, A]
  RequestConverter: Request => Either[Err, A] (e.g. parse from body)

Performing conversion:
extract all segments, end up with e.g. String :: String :: Request
 */

