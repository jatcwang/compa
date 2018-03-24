package com.skedulo.htplay.paths

/**
  * Typeclass providing the "URL Not Matched" value of the error type Err.
  * The value provided by uriNotMatched is returned when a path failed to match
  */
trait HasUriNotMatched[Err] {
  def uriNotMatched: Err
}
