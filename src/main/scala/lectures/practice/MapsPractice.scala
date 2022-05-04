package lectures.practice

object MapsPractice extends App {

  /*
  Write a function using map which :
    Takes a String “macbooks”
    Maps on each char to make them uppercase, and returns “MACBOOKS ARE THE BEST”
  */

  val macbookFunction: (String) => String = word => {
   val capitalisedWord = word.toList.map(_.toUpper).mkString("")
    s"${capitalisedWord} ARE THE BEST"
  }

println(macbookFunction("macbooks"))

/*
Write a function which
- Turns a list of strings to ints e.g. List(“1”, “2”, “3”) -> List(1,2,3)
- Multiplies each int by 2
- Finds the sum of the List

*/

val stringMultiplyAndSum: (List[String]) => Int = inputStringList => {
  inputStringList.map(x => x.toInt * 2).sum
}

}
