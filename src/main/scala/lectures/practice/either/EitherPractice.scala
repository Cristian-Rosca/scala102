package lectures.practice.either

object EitherPractice extends App {

  //  def myEitherFunction (username: String, password: String): Either[Exception, Boolean] = { // specify the datatype of the Left first
  //    try {
  //      Right(userExists(username, password))
  //    } catch {
  //      case e: Exception => Left(e)
  //    }
  //  }

  def userExists(username: String, password: String): Boolean = {
    (username, password) match {
      case ("Boris Johnson", "pword123") => true
      case ("Barack Obama", "merica") => true
      case _ => false
    }
  }

  def myEitherFunction(username: String, password: String): Either[Exception, Boolean] = {
    if (userExists(username, password) == false) Left(Exception("UserNotFoundException"))
    else Right(userExists(username, password))
  }

  println(myEitherFunction("lol", "123"))

  /*
Work out what the following function calls will return.

EitherStyle.parse("23").isRight
EitherStyle.parse("twenty").isLeft
EitherStyle.parse(100).isLeft
EitherStyle.parse(100.toString).isLeft
EitherStyle.parse("21.5").isRight
*/

  object EitherStyle {
    def parse(s: String): Either[NumberFormatException, Int] =
      if (s.matches("-?[0-9]+")) Right(s.toInt)
      else Left(new NumberFormatException(s"${s} is not a valid integer"))
  }

  println(EitherStyle.parse("23").isRight) // -> True
  println(EitherStyle.parse("twenty").isLeft) // -> True
  //  println(EitherStyle.parse(100).isLeft) // -> Won't compile
  println(EitherStyle.parse(100.toString).isLeft) // -> False
  println(EitherStyle.parse("21.5").isRight) // -> False

}
