package com.skedulo.htplay.paths

import com.skedulo.htplay.paths.Converter.{AnyConverter, ExistConverter}
import org.http4s.{Request, Response}
import shapeless.{Generic, HList, HNil}
import shapeless.syntax.std.traversable._

import scala.collection.{mutable => mut}
import scala.language.higherKinds



object Matchers {

  def makeMatcher[F[_], Err, Vars <: HList](converters: Vector[ExistConverter[Err]], matchPathSegments: Vector[Segment])(implicit E: HasUriNotMatched[Err]) = {
    new Matcher[F, Err, Vars] {
      override def processReq(req: Request[F]): Either[Err, Vars] = {
        val thisPathSegments = req.uri.path.split('/').filter(_.nonEmpty)
        if (thisPathSegments.length == matchPathSegments.length) {
          var idx = 0
          var cont = true
          val pathInputValues = mut.ArrayBuffer.empty[Any]
          while (idx < thisPathSegments.length) {
            val thisSeg = thisPathSegments(idx)
            matchPathSegments(idx) match {
              case LiteralSegment(str) => if (str != thisSeg) {
                cont = false
              }
              case p: PathVarSegment[err, a] => {
                pathInputValues += thisSeg
              }
            }
            idx += 1
          }
          if (cont) {
            // verified at least all literal segments have been matched
            val convIter = converters.iterator
            val processedValues = mut.ArrayBuffer.empty[Any]
            // process all values we already have
            pathInputValues.foreach { thisValue =>
              //TODOO: unsafe parse
              processedValues += convIter.next().asInstanceOf[AnyConverter[Err]].converter(thisValue).right.get
            }
            // Other processors such as query strings or body parsers
            //TODOO: assuming all the rest of the converters take a request
            val qs = req.uri.query
            convIter.foreach { conv =>
              val convResult = conv match {
                //TODOO: unsafe gets
                case QueryStringConverter(f) => f(qs).right.get
                case RequestConverter(f) => f(req.asInstanceOf[Request[Any]]).right.get
                case StringConverter(_) => throw new Exception("not expecting StringConverter when converting things after the path variables")
              }
              processedValues += convResult
            }
            var res: HList = HNil
            processedValues.reverseIterator.foreach { thisRes =>
              res = thisRes :: res
            }
            Right(res.asInstanceOf[Vars])
          }
          else Left(E.uriNotMatched)
        }
        else {
          Left(E.uriNotMatched)
        }
      }
    }
  }
}

trait Matcher[F[_], Err, Vars <: HList] {
  def processReq(req: Request[F]): Either[Err, Vars]
}
