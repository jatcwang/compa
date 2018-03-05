package com.skedulo.htplay.paths

trait HasUriNotMatched[Err] {
  def uriNotMatched: Err
}
