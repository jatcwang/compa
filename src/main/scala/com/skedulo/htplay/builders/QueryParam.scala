package com.skedulo.htplay.builders

import com.skedulo.htplay.easy.{InvalidRequest, ReqError}
import cats.syntax.either._

sealed trait QueryParam[Err, T]
//TODOO: do we need this? we can probably generalize this to use MultiParam (and call headoption)
case class SingleParam[Err, T](key: String, convert: String => Either[Err, T]) extends QueryParam[Err, T]
// indicate that we shouldn't ignore multiple params
case class MultiParam[Err, T](key: String, convert: Seq[String] => Either[Err, T]) extends QueryParam[Err, T]

object QueryParam {
  def int(key: String) = SingleParam[ReqError, Int](key, str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage)))
  def str(key: String) = SingleParam[ReqError, String](key, str => Right(str))
}
