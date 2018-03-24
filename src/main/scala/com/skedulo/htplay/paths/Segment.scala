package com.skedulo.htplay.paths

private[htplay] sealed trait Segment

// Literal matching of a segment
private[htplay] case class LiteralSegment(str: String) extends Segment
// a path variable may convert to multiple parameters
//TODOO: make you able to pass a name
private[htplay] case class PathVarSegment[Err, A](parser: String => Either[Err, A]) extends Segment
