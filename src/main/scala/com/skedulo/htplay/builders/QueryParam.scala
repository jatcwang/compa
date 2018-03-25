package com.skedulo.htplay.builders

import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._

final case class QueryParam[Err, T](key: String, runConvert: List[String] => Either[Err, T])

object QueryParam {

  type Key = String
  /**
    * A single, required query parameter
    */
  def single[Err, T](key: Key, runConvert: String => Either[Err, T], handleMissing: Key => Err): QueryParam[Err, T] = {
    val func = (strs: List[String]) => {
      strs match {
        case Nil => Left(handleMissing(key))
        case first :: _ => runConvert(first)
      }
    }
    QueryParam(key, func)
  }

  /**
    * Expects one or more query parameters
    */
  def many[Err, T](key: Key, convertFn: String => Either[Err, T], handleMissing: Key => Err): QueryParam[Err, List[T]] = {
    val f = (strs: List[String]) => {
      strs match {
        case Nil => Left(handleMissing(key))
        case nonEmpty => nonEmpty.traverse(convertFn)
      }
    }

    QueryParam(key, f)
  }

  //TODOO: also add default
  def optional[Err, T](key: Key, convertFn: String => Either[Err, T]): QueryParam[Err, Option[T]] ={
    val f = (strs: List[String]) => {
      strs match {
        case Nil => Right(None)
        case first :: _ => convertFn(first).map(Some(_))
      }
    }
    QueryParam(key, f)
  }
}
