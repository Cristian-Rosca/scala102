package lectures.practice.options

object OptionsPractice extends App {

  // write a case class for a chocolate bar with optional filling
  // write a function that returns filling of chocolate bar

  case class Chocolate (name: String, filling: Option[String])

  val mars = Chocolate ("Mars", Some("Caramel"))

  def returnFilling (chocolate: Chocolate): String = {
    chocolate.filling match {
      case Some(value) => value
      case None => "no filling"
    }
  }

  println(returnFilling(mars))

  // create a dog case class with optional parameters
  // write a function to return what colour spots the dog has -> case/match and getOrElse implementations

  case class Dog (name: String, age: Int, spotColour: Option[String], likeTreats: Option[Boolean])

  val darcy = Dog("Darcy", 12, Some("white"), Some(true))

  def whatColourSpots(dog: Dog): String = {
    dog.spotColour match {
      case Some(value) => value
      case None => "dog has no spots"
    }
  }

  def whatColourSpotsAlt(dog: Dog): String = {
    dog.spotColour.getOrElse("dog has no spots")
    }


// write a function that takes an optional INT and either returns the int * 2: Some(doubledValue), or None if the value is not supplied

def doubleIfInt(input: Option[Int]) = {
  input match {
    case Some(value) => value * 2
    case None => None
  }
}

}
