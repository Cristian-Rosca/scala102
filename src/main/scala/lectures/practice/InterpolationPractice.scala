package lectures.practice

object InterpolationPractice extends App {

  val amy = "Amy"
  val kevin = "Kevin"

  println(s"$kevin is older then $amy")

  val amyAge = 20.3482789
  val kevinAge = 30.628172

  println(s"$kevin is ${kevinAge - amyAge} year older then $amy")

  // combining s and f interpolation
  println(s"$kevin is " + f"${kevinAge - amyAge}%.1f" + s" years older than $amy")


  // Use string interpolation to avoid the new lines from this string. “\n\n\n\n\n\n\n\nsameline\n\n”

  println(raw"\n\n\n\n\n\n\n\nsameline\n\n")

  case class Person (name: String, age: Float, favFruits: List[String])

  val gary = Person("Gary", 22.2392, List("apples, oranges"))

  println(s"Hi my name is ${gary.name}" + f" and I am ${gary.age}%.1f" + raw" . My favourite fruits are ${gary.favFruits}")
}
