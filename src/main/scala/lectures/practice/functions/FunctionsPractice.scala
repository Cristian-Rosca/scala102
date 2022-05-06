package lectures.practice.functions

object FunctionsPractice extends App {

  val pi = 3.14

  // method to calculate radius
  //  def calculateRadius (circumference: Double) = circumference / (2 * pi)
  //
  //  method to calculate the area of a circle
  //  def calculateArea (radius: Double) = pi * (radius * radius)
  //
  //  method to calculate circumference
  //  def calculateCircumference (radius: Double) = 2 * pi * radius


  // function to calculate circumference
  val calcCircumference: (Double) => Double = radius => 2 * pi * radius // doesn't need parameters so you can pass into HOF

  // function to calculate the area of a circle
  val calcArea: (Double) => Double = radius => pi * (radius * radius)

  // function that takes a list of radii as a param and either the area or circumference functions
  // outputs a list of circle areas or circumferences
  def myHOF(function: (Double) => Double, list: List[Double]) = {
    list.map(function)
  }

  val myRadiusList = List(1.00, 2.00)

  println(myHOF(calcCircumference, myRadiusList))


}
