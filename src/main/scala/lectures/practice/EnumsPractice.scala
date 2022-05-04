package lectures.practice

import lectures.practice.EnumsPractice.Month.{January, February, March, April, May, June, July, August, September, October, November, December}

object EnumsPractice extends App {

  object Months extends Enumeration {

    val January = Value("Jan")
    val February = Value("Feb")
    val March = Value("Mar")
    val April = Value("Apr")
    val May = Value("May")
    val June = Value("Jun")
    val July = Value("Jul")
    val August = Value("Aug")
    val September = Value("Sep")
    val October = Value("Oct")
    val November = Value("Nov")
    val December = Value("Dec")

  }

  println(Months.values)


  sealed trait Month

  object Month {
    case object January extends Month

    case object February extends Month

    case object March extends Month

    case object April extends Month

    case object May extends Month

    case object June extends Month

    case object July extends Month

    case object August extends Month

    case object September extends Month

    case object October extends Month

    case object November extends Month

    case object December extends Month
  }

  // print out all the values of the enum now, what approach could we use?

val caseObjectMonths = Seq(January, February, March, April, May, June, July, August, September, October, November, December)
println(caseObjectMonths)
}
