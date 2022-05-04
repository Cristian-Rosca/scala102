package lectures.practice

import lectures.practice.ExtraEnumsPractice.TypeOfAnimalTrait.{Bird, Mammal, Reptile}

object ExtraEnumsPractice extends App {

  /*

  Create a case class to represent an animal and include as many enums as you can think of to describe the animal.
  • E.g. TypeOfAnimalEnum - Reptile, Bird, Mammal etc or CanFlyEnum - Yes, No, SortOf (chickens, penguins :P)

  Firstly use native enumeration and then make a copy using sealed case objects.
  */

  // Enum method
  case class Animal (name: String, typeOfAnimal: TypeOfAnimalEnum.Value, canFly: CanFly.Value)

  object TypeOfAnimalEnum extends Enumeration {
    val Reptile, Bird, Mammal = Value
  }

  object CanFly extends Enumeration {
    val Yes, No, Maybe = Value
  }

  val crocodileEnum = Animal("Bob", TypeOfAnimalEnum.Reptile, CanFly.No)
  val catEnum = Animal("Mary", TypeOfAnimalEnum.Mammal, CanFly.No)

  def animalTypeIdentifier (animal: Animal) = {
    animal.typeOfAnimal match {
      case TypeOfAnimalEnum.Reptile => println("I'm a reptile")
      case TypeOfAnimalEnum.Mammal => println("I'm a mammal")
      case TypeOfAnimalEnum.Bird => println("I'm a bird")
    }
  }

  // Case Class method
  case class AnimalAlt (name: String, typeOfAnimal: TypeOfAnimalTrait, canFly: CanFlyTrait )

  sealed trait TypeOfAnimalTrait

  object TypeOfAnimalTrait {
    case object Reptile extends TypeOfAnimalTrait
    case object Bird extends TypeOfAnimalTrait
    case object Mammal extends TypeOfAnimalTrait
  }

  sealed trait CanFlyTrait

  object CanFlyTrait {
    case object Yes extends CanFlyTrait
    case object No extends CanFlyTrait
    case object Maybe extends CanFlyTrait
  }

  val altCrocodile = AnimalAlt("Rob", TypeOfAnimalTrait.Reptile, CanFlyTrait.Yes)

  val altCat = AnimalAlt("Kat", TypeOfAnimalTrait.Mammal, CanFlyTrait.Yes)

  def altAnimalTypeIdentifier (animal: AnimalAlt) = {
    animal.typeOfAnimal match {
      case Reptile => println("I'm a reptile")
      case Mammal => println("I'm a mammal")
      case Bird => println("I'm a bird")
    }
  }


  /*

  Populate your case class with values and then use pattern matching to describe the animal.
  Which style of enumeration do you prefer? Why?

  E.g. val crocodile = AnimalCaseClass("Crok", AnimalType.Reptile, CanFly.No)
    crocodile match {
      case something => println(“I’m a reptile”)
    }

  */

}
