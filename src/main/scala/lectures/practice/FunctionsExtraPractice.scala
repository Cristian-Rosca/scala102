package lectures.practice

import scala.util.Random

object FunctionsExtraPractice extends App {

  // when given an optional spongebob character, returns a spongebob quote matching on the name.
  // Provide a default quote where no character is supplied.
  // See http://www.spongebobquotes.org


  val spongeBobQuoteGen: (Option[String]) => String = character => character match {
    case Some("spongebob") => "Spongebob quote"
    case Some("squidward") => "Squidward quote"
    case Some("patrick") => "Patrick quote"
    case Some("sandy") => "Sandy quote"
    case Some("gary") => "meow"
    case _ => "default quote"
  }

  //accepts an optional quote to return.
  // If no quote is supplied return a random (use scala.util.Random) philosophy quote.
  // See https://www.philosophybasics.com/general_quotes.html

  val philQuotes = List("The unexamined life is not worth living", "Whereof one cannot speak, thereof one must be silent", "Entities should not be multiplied unnecessarily" )

val returnQuoteOrPhilosophy: (Option[String]) => String = quote => quote match {
  case None => philQuotes(Random.nextInt(philQuotes.size))
  case Some(value) => value
}


  // Create a higher order function that accepts either of the quote functions above.




  def nonsenseHOF(function: (Option[String]) => String, myStr: Option[String]) = {
    function match {
      case returnQuoteOrPhilosophy => function(myStr)
      case spongeBobQuoteGen => function(myStr)
      case _ => "enter a valid function"
    }
  }

println(nonsenseHOF(returnQuoteOrPhilosophy,None))

}
