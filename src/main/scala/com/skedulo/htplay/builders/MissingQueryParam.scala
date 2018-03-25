package com.skedulo.htplay.builders

/**
  * {{{
  *   "blah".as[Int],
  * }}}
  */

trait MissingQueryParam[Err] {
  def handle(key: String): Err
}

