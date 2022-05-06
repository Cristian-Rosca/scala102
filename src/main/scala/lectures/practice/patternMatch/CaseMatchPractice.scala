package lectures.practice.patternMatch

object CaseMatchPractice extends App {

  val flavour: String = "Chocolate"

  flavour match {
    case "Chocolate" => "chocolate fudge brownie"
    case "Caramel" => "caramel chew chew"
    case "Cookie" => "cookie dough"
    case _ => flavour
  }

  println("What size pizza would you like?")
  var pizzaSize = scala.io.StdIn.readLine()

  pizzaSize match {
    case "7" => println("personal")
    case "9" => println("small")
    case "11" => println("medium")
    case "13" => println("large")
  }


  // Q3

  var cost = 0

  trait Stuffed

  case class Pizza(size: Int, isStuffed: Boolean)

  var customerOrder = Pizza(7, false)

  customerOrder.size match {
    case 7 => println("personal")
      var cost = 5.99
    case 9 => println("small")
      var cost = 10.99
    case 11 if customerOrder.isStuffed == true => println("medium with stuffed crust")
      var cost = 12.99 + 2.99
    case 11 => println("medium")
      var cost = 12.99
    case 13 if customerOrder.isStuffed == true => println("large with stuffed crust")
      var cost = 14.99 + 2.99
    case 13 => println("large")
      var cost = 14.99
    case _ => println("medium")
      var cost = 12.99
  }


  def cityCapitaliser(city: String): String = {
    city.toLowerCase() match {
      case "london" => city.toUpperCase()
      case "belfast" => city.toUpperCase()
      case "Cardiff" => city.toUpperCase()
      case "Edinburgh" => city.toUpperCase()
      case _ => "Isn't a UK capital"

    }
  }


  // Write a animal abstract class

  abstract class Animal {
    val name: String
    val age: Int
  }

  case class Dog(name: String, age: Int) extends Animal

  case class Cat(name: String, age: Int) extends Animal

  var darcy = Dog("darcy", 12)

  def animalTypeCheck(animal: Animal) = {
    animal.getClass.toString match {
      case "class lectures.part1basics.CaseMatchPractice$Dog" if animal.name.toLowerCase == "sam" => println(s"Sam's age is ${animal.age}")
      case "class lectures.part1basics.CaseMatchPractice$Dog" => println("Dog")
      case "class lectures.part1basics.CaseMatchPractice$Cat" => println("Cat")
      case _ => println("other")
    }
  }


  def isSame(animal: Animal) = {
    animal.name.toLowerCase() match {
      case "sam" => println(s"Sam's age is ${animal.age}")
      case _ => println("this is not sam")
    }
  }

  // doing this with case/match would be too long?
  def animalAgeChecker(animal: Animal) = {
    if (animal.age > 10) {
      println(s"${animal.name} is ${animal.age} years old")
    }
    else {
      println(s"${animal.name} is young")
    }
  }

  // tried doing this using case/match here?
  def animalAgeChecker2(animal: Animal) = {
    val isYoung = if (animal.age > 10) false else true

    isYoung match {
      case false => println(s"${animal.name} is young")
      case true => println(s"${animal.name} is ${animal.age} years old")
    }
  }


}
