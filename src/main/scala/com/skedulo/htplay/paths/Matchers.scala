package com.skedulo.htplay.paths

import com.skedulo.htplay.builders.Converter.{AnyConverter, ExistConverter}
import com.skedulo.htplay.builders._
import org.http4s.{Method, Request}
import shapeless.{HList, HNil}

import scala.collection.{mutable => mut}
import scala.language.higherKinds

object Matchers {

  def makeMatcher[F[_], Err, Vars <: HList](method: Method, converters: Vector[ExistConverter[Err]], matchPathSegments: Vector[PathSegment])(implicit E: HasUriNotMatched[Err]) = {
    new Matcher[F, Err, Vars] {
      override def processReq(req: Request[F]): Either[Err, Vars] = {
        if (req.method == method) {
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
              // Attempt to process all values we already have from path variables
              var pathVarParseErr: Option[Err] = None
              var jdx = 0
              // TODO: should handle fallbacks (e.g. /mypath/{int}/ then fallback to /mypath/{string}/)
              while (pathVarParseErr.isEmpty && jdx < pathInputValues.length) {
                //TODO: asInstanceOf?
                convIter.next().asInstanceOf[AnyConverter[Err]].run(pathInputValues(jdx)) match {
                  case Right(res) => {
                    processedValues += res
                  }
                  case Left(e) => {
                    pathVarParseErr = Some(e)
                  }
                }
                jdx += 1
              }
              pathVarParseErr match {
                case Some(e) => Left(e)
                case None => {
                  // Other processors such as query strings or body parsers
                  //TODO: assuming all the rest of the converters take a request
                  val qs = req.uri.query
                  convIter.foreach { conv =>
                    val convResult = conv match {
                      //TODO: unsafe gets
                      case QueryStringConverter(f) => {
                        f(qs).right.get
                      }
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
              }
            }
            else Left(E.uriNotMatched)
          }
          else {
            Left(E.uriNotMatched)
          }
        }
        else Left(E.uriNotMatched)
      }
    }
  }
}

trait Matcher[F[_], Err, Vars <: HList] {
  def processReq(req: Request[F]): Either[Err, Vars]
}
