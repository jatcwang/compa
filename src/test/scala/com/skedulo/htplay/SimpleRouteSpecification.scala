package com.skedulo.htplay

import com.skedulo.htplay.builders.RouteSpecification
import com.skedulo.htplay.simple.{InvalidRequest, ReqError}
import com.skedulo.htplay.utils.FromStringInstances
import monix.eval.Task

trait SimpleRouteSpecification extends RouteSpecification[Task, ReqError] with FromStringInstances[ReqError] {

  override def handleInvalidQueryParam(msg: String, e: Option[Throwable]): ReqError = InvalidRequest(msg)
  override def handleMissingQueryParam(key: String): ReqError = InvalidRequest(s"Missing $key")

}
