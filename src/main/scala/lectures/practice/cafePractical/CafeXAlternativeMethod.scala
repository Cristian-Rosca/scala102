package lectures.practice.cafePractical

object CafeXAlternativeMethod extends App {

  class MenuItem {
    val itemName: String = ""
    val cost: BigDecimal = 0
  }

  object Cola extends MenuItem with Cold with Drink {
    override val itemName: String = "Cola"
    override val cost: BigDecimal = 0.5
  }

  object Coffee extends MenuItem with Hot with Drink {
    override val itemName: String = "Coffee"
    override val cost: BigDecimal = 1.00
  }

  object CheeseSandwich extends MenuItem with Cold with Food {
    override val itemName: String = "Cheese Sandwich"
    override val cost: BigDecimal = 2.20
  }

  object SteakSandwich extends MenuItem with Hot with Food {
    override val itemName: String = "Steak Sandwich"
    override val cost: BigDecimal = 4.50
  }

  trait Drink {}

  trait Food {}

  trait isPremium {}

  trait Hot {}

  trait Cold {}


  def billCalculator(customerOrder: List[MenuItem]) = {
    val price = customerOrder.map(x => x.cost).sum
    println(s"The cost of your items is: £${price}")


  }

  val order: List[MenuItem] = List(Coffee, SteakSandwich)



  // try to loop through the list and examine if there are any items that extend Food and Hot


}
