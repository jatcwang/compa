package com.skedulo.htplay.utils
import cats.syntax.either._

/**
  * Wrapper class a function that attempts to convert a String to T, or fail with an Err
  */
final case class FromString[Err, T](run: String => Either[Err, T])

object FromString {
}


//TODOO: better name and/or collect all error handling overrides for better UX?
trait FromStringInstances[Err] {

  def handleFromStringError(msg: String, e: Option[Throwable]): Err

  implicit val intFromString: FromString[Err, Int] = {
    val f = (str: String) => {
      Either.catchNonFatal(str.toInt).leftMap(e => handleFromStringError(e.getMessage, Some(e)))
    }
    FromString(f)
  }

  implicit val strFromString: FromString[Err, String] = {
    FromString((str: String) => Right(str))
  }

}
