package com.skedulo.htplay.builders

private[htplay] sealed trait PathSegment

// Literal matching of a segment
private[htplay] case class LiteralSegment(str: String) extends PathSegment
// a path variable may convert to multiple parameters
//TODO: make you able to pass a name
private[htplay] case class PathVarSegment[Err, A](parser: String => Either[Err, A]) extends PathSegment
