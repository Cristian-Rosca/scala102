package lectures.practice.caseClass

object CaseClassPractice extends App {

  case class Dog(name: String, breed: String, age: Int)

  val darcy = Dog("Darcy", "Italian Greyhound", 12)
  val lady = Dog("Lady", "Italian Greyhound", 12)
  val cristian = Dog("Cristian", "Italian Greyhound", 12)
  val caoimhe = Dog("caoimhe", "Shiba", 12)

  case class Kennel(var name: String, var inhabitants: List[Object])

  val kennel = Kennel("Kennel", List(darcy, lady, cristian, caoimhe))


  var upgradedKennel = kennel.copy(inhabitants = List(darcy, lady, cristian, caoimhe, cat, bird))

  case class Cat(name: String, breed: String, age: Int)

  case class Bird(name: String, breed: String, age: Int)

  val cat = Cat("Cat", "Cat", 12)
  val bird = Bird("Bird", "Bird", 12)


}
