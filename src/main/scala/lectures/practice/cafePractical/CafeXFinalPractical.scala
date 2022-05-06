package lectures.practice.cafePractical

import lectures.practice.cafePractical.CafeXFinalPractical.FoodOrDrink._
import lectures.practice.cafePractical.CafeXFinalPractical.HotOrCold._

object CafeXFinalPractical extends App {

  // Food Items
  case class MenuItem(name: String, foodOrDrink: FoodOrDrink, hotOrCold: HotOrCold, cost: Double, isPremium: Boolean)

  sealed trait HotOrCold

  object HotOrCold {
    case object Hot extends HotOrCold

    case object Cold extends HotOrCold
  }

  sealed trait FoodOrDrink

  object FoodOrDrink {
    case object Food extends FoodOrDrink

    case object Drink extends FoodOrDrink
  }

  // Menu
  val cola = MenuItem("Cola", Drink, Cold, 0.50, false)
  val coffee = MenuItem("Cola", Drink, Hot, 1.00, false)
  val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
  val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 2.00, false)
  val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)


  // Customers
  case class Customer(name: String, loyaltyStars: Int)

  // Sample Customer
  val karen = Customer("Karen", 10)


  // Bill Calculator
  // Takes a list of ordered items and the customer
  def billCalculator(customerOrder: List[MenuItem], customer: Customer) = {
    // Sum of cost of all items in order list
    val price = customerOrder.map(x => x.cost).sum

    // Calculates the loyalty discount for the customer
    val loyaltyDiscount = {
      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      (price - nonDiscountablePrice) * loyaltyDiscountPercentage
    }

    // Full order price - Loyalty discount
    val loyaltyPrice = price - loyaltyDiscount

    // Calculates rate that Service Charge is based on
    val tipMultiplier = {
      if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 20% service charge if premium food in order
      else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
      else if (customerOrder.find(x => FoodOrDrink == Food).isDefined) 0.1 // 10% service charge if food in order
      else 0 // No service charge if no items are foods
    }

    // Service Charge
    // Calculated based on loyaltyPrice (Full order price - Loyalty discount)
    val serviceCharge: Double = {
      if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined)
        if (loyaltyPrice * tipMultiplier >= 40) 40 // Maximum of £40 if premium items in order
        else loyaltyPrice * tipMultiplier
      else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined)
        if (loyaltyPrice * tipMultiplier >= 20) 20 // Maximum of £40 if premium items in order
        else loyaltyPrice * tipMultiplier
      else loyaltyPrice * tipMultiplier
    }

    val totalCharge = loyaltyPrice + serviceCharge

    // Statements to print customer bill
    println(s"Thank you for shopping with us, ${customer.name}!")

    println("Please see your order summary below")
    println("---------")
    println("*Premium Items*")
    for (element <- premiumItemNames) {
      println(element)
    }

    println("---------")
    println("*Food*")
    for (element <- foodNames) {
      println(element)
    }

    println("---------")
    println("*Drinks*")
    for (element <- drinkNames) {
      println(element)
    }


    println("---------")
    println(f"The cost of your items is: £$price%.2f")
    println(f"Based on ${customer.loyaltyStars} loyalty stars, you received a loyalty discount of: £$loyaltyDiscount%.2f")
    println(f"The service charge is: £$serviceCharge%.2f")
    println("---------")
    println(f"The total for the order is: £$totalCharge%.2f")

  }

  // Sample customer order
  val order = List(cheeseSandwich, steakSandwich, lobster)

  // Creates a list of premium items in the order to be printed on receipt
  val premiumItemNames = {
    val premiumItems = order.filter(x => (x.isPremium == true))
    premiumItems.map(x => x.name)
  }

  // Creates a list of premium items in the order to be printed on receipt

  val foodNames = {
    val foodItems = order.filter(x => (x.isPremium == false) && (x.foodOrDrink == Food))
    foodItems.map(x => x.name)
  }

  val drinkNames = {
    val drinkNames = order.filter(x => (x.isPremium == false) && (x.foodOrDrink == Drink))
    drinkNames.map(x => x.name)
  }

  billCalculator(order, karen)


}
