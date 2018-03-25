package com.skedulo.htplay.builders

import com.skedulo.htplay.utils.{FromString, FromStringInstances}
import org.http4s.Method
import shapeless._

/**
    * Specifies the effect type F and error type Err
    */
trait RouteSpecification[F[_], Err] extends FromStringInstances[Err] {

  def handleInvalidQueryParam(msg: String, e: Option[Throwable]): Err
  def handleMissingQueryParam(key: String): Err

  final override def handleFromStringError(msg: String, e: Option[Throwable]): Err = handleInvalidQueryParam(msg, e)

  implicit val missingQueryParam: MissingQueryParam[Err] = new MissingQueryParam[Err] {
    override def handle(key: String): Err = handleMissingQueryParam(key)
  }

  class Http4sMethodOps(val method: Method) {

    def /(segment: String): PathBuilder[F, Err, HNil] =
      PathBuilder[F, Err, HNil](method, matchSegments = Vector(LiteralSegment(segment)), converters = Vector.empty)

    def /[A](segment: PathVarSegment[Err, A]): PathBuilder[F, Err, A :: HNil] = {
      val c = StringConverter(segment.parser)
      PathBuilder[F, Err, A :: HNil](method, matchSegments = Vector(segment), converters = Vector(c))
    }
  }

  implicit def toMethodOps(method: Method) = new Http4sMethodOps(method)

  class StringQueryParamOps(val str: String) {
    //TODOO: need some way to check for invalid variables
    def as[T](implicit fromStr: FromString[Err, T], missing: MissingQueryParam[Err]): QueryParam[Err, T] = {
      QueryParam.single(str, fromStr.run, missing.handle)
    }
  }

  implicit def toStringQueryParamOps(str: String) = new StringQueryParamOps(str)

}
