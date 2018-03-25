package com.skedulo.htplay.builders

import org.http4s.{Query, Request}

sealed trait Converter[In, Err, A] {
  def run: In => Either[Err, A]
}

object Converter {
  type ExistConverter[Err] = Converter[_, Err, _]
  type AnyConverter[Err]   = Converter[Any, Err, Any]
}

// Convert a string into A
case class StringConverter[Err, A](override val run: String     => Either[Err, A]) extends Converter[String, Err, A]
case class QueryStringConverter[Err, A](override val run: Query => Either[Err, A])
    extends Converter[Query, Err, A]
case class RequestConverter[F[_], Err, A](override val run: Request[F] => Either[Err, A])
    extends Converter[Request[F], Err, A]