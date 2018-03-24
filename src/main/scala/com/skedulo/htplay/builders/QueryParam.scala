package com.skedulo.htplay.builders

import com.skedulo.htplay.simple.{InvalidRequest, ReqError}
import cats.syntax.either._

sealed trait QueryParam[Err, T]

object QueryParam {

  final case class SingleParam[Err, T](key: String, convert: String => Either[Err, T]) extends QueryParam[Err, T]
  // indicate that we shouldn't ignore multiple parameters of the same name
  final case class MultiParam[Err, T](key: String, convert: Seq[String] => Either[Err, T]) extends QueryParam[Err, T]

  def int(key: String) = SingleParam[ReqError, Int](key, str => Either.catchNonFatal(str.toInt).leftMap(e => InvalidRequest(e.getMessage)))
  def str(key: String) = SingleParam[ReqError, String](key, str => Right(str))
}
