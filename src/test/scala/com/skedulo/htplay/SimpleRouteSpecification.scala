package com.skedulo.htplay

import com.skedulo.htplay.builders.RouteSpecification
import com.skedulo.htplay.simple.{FilterError, InvalidRequest, ReqError, UriNotMatched}
import com.skedulo.htplay.utils.FromStringInstances
import monix.eval.Task
import org.http4s.{Response, Status}

trait SimpleRouteSpecification extends RouteSpecification[Task, ReqError] with FromStringInstances[ReqError] {

  override def handleInvalidQueryParam(msg: String, e: Option[Throwable]): ReqError = InvalidRequest(msg)
  override def handleMissingQueryParam(key: String): ReqError                       = InvalidRequest(s"Missing $key")

  override def errorToResponse(err: ReqError): Task[Option[Response[Task]]] = {
    err match {
      case UriNotMatched => Task.now(None)
      case InvalidRequest(msg) => {
        Response[Task](Status.BadRequest).withBody(msg).map(Some(_))
      }
      case FilterError(e) => Task.now(Some(Response(Status.InternalServerError)))
    }
  }

}
