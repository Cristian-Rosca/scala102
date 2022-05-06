package lectures.practice.maps

object ExtraMapsPractice extends App {

  /*
  Write a function that accepts an optional Int and multiplies the integer by 12, if nothing is supplied return 12
  */

  def multiplyBy12orReturn: (Option[Int]) => Int = input => {
    input match {
      case None => 12
      case Some(value) => value * 12
    }
  }

  /*
  The following value is created to model recent exam results. Where a None means that the student did not attend.

  val scores = Seq(None, Some("A"), Some("B"), Some("C"), None, Some(“F”))
    -> What keyword could be used to only produce a list of results of those students that attended?
    -> Using map turn the scores of the students that didn’t attend into an F.
        E.g. should result in List(F, A, B, C, F, F)

  */

  val scores = Seq(None, Some("A"), Some("B"), Some("C"), None, Some("F"))

  val didAttend = scores.filter(_.isDefined)
  val didNotAttend = scores.filter(_.isEmpty)


  var updatedScores = scores.flatMap { case None => Option("F"); case x => x }

  println(updatedScores)

}
