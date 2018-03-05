package com.skedulo.htplay.paths

import cats.data.ValidatedNel
import org.http4s.Uri

sealed trait ReqError

case object UriNotMatched extends ReqError

// Need better error details
final case class InvalidRequest(errorMsg: String) extends ReqError

// Exception was thrown when running a filter or processing request
final case class FilterError(e: Throwable) extends ReqError

object ReqError {
  implicit val hasUriNotMatched: HasUriNotMatched[ReqError] = new HasUriNotMatched[ReqError] {
    override def uriNotMatched: ReqError = UriNotMatched
  }
}
