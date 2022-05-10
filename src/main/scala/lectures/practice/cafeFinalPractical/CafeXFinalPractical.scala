package lectures.practice.cafeFinalPractical

import lectures.practice.cafeFinalPractical.Currency.GBP
import lectures.practice.cafeFinalPractical.Currency.EUR
import lectures.practice.cafeFinalPractical.Currency.USD
import lectures.practice.cafeFinalPractical.FoodOrDrink.*
import lectures.practice.cafeFinalPractical.HotOrCold.*
import lectures.practice.cafeFinalPractical.MenuItem
import lectures.practice.cafeFinalPractical.Customer

import java.time.*
import java.util.Calendar


object CafeXFinalPractical extends App {

  // Menu
  val cola = MenuItem("Cola", Drink, Cold, 0.50, false)
  val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
  val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
  val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 2.00, false)
  val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)

  //Customers
  val karen = Customer("Karen", 10, 4321)
  val bob = Customer("Bob", 5, 2001)

  // Get local time in hours
  var timeNow = LocalTime.now.getHour()


  // Bill Calculator
  // Takes a list of ordered items and the customer
  def billCalculator(customerOrder: List[MenuItem], customer: Customer, currency: Currency) = {
    if (customerOrder.isEmpty) println("No items added to customer order. Please add items and try again") else {
      // Update exchange rates here if necessary - can import live fx rates as an extension
      val currencyMultiplier = {
        currency match {
          case GBP => 1
          case EUR => 1.15
          case USD => 1.23
          case _ => 1
        }
      }

      // Changes currency symbol in receipt
      val currencySymbol = {
        currency match {
          case GBP => "£"
          case EUR => "€"
          case USD => "$"
          case _ => "£"
        }
      }

      // Sum of cost of all items in order list (adjusted based on selected currency)
      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      // Calculates the loyalty discount for the customer
      val loyaltyDiscount = {
        val loyaltyDiscountPercentage =
          if (customer.loyaltyStars < 3) 0
          else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
          else 0.2

        // Premium Items are not discountable
        val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
        val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

        (price - nonDiscountablePrice) * loyaltyDiscountPercentage
      }

      val happyHourDiscount = {
        // Get total cost of drinks in order
        val drinksOrderCost = customerOrder.filter(x => (x.foodOrDrink == Drink)).map(x => x.cost).sum * currencyMultiplier

        // If local hours is between 18 and 21 happyHourDiscount is half of drink prices, else it is 0
        if ((timeNow > 17) && (timeNow < 22)) drinksOrderCost * 0.5
        else 0
      }

      // Full order price - Loyalty discount - happy hour discount (if applicable)
      val finalPrice = price - loyaltyDiscount - happyHourDiscount

      // Calculates rate that Service Charge is based on
      val tipMultiplier = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 25% service charge if premium food in order
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
        else if (customerOrder.find(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food)).isDefined) 0.1 // 10% service charge if food in order
        else 0 // No service charge if no items are foods
      }

      // Service Charge
      // Calculated based on finalPrice (Full order price - Loyalty discount)
      val serviceCharge: Double = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined)
          if (finalPrice * tipMultiplier >= 40) 40 // Maximum of £40 if premium items in order
          else finalPrice * tipMultiplier
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined)
          if (finalPrice * tipMultiplier >= 20) 20 // Maximum of £40 if premium items in order
          else finalPrice * tipMultiplier
        else finalPrice * tipMultiplier
      }

      val totalCharge = finalPrice + serviceCharge



      // Statements to print customer bill
      println(s"Thank you for shopping with us, ${customer.name}!")

      // Print names of premium items ordered
      println("Please see your order summary below")
      println("---------")
      println("Premium Items")
      for (element <- premiumItemNames) {
        println(s"• ${element}")
      }

      // Print names of food items ordered
      println("---------")
      println("Food")
      for (element <- foodNames) {
        println(s"• ${element}")
      }

      // Print names of drinks ordered
      println("---------")
      println("Drinks")
      for (element <- drinkNames) {
        println(s"• ${element}")
      }

      println("---------")
      println(f"The cost of your items is: $currencySymbol$price%.2f")
      println(f"Based on ${customer.loyaltyStars} loyalty stars, you received a loyalty discount of: $currencySymbol$loyaltyDiscount%.2f")
      if ((timeNow > 17) && (timeNow < 22)) println(f"The happy hour discount is: $currencySymbol$happyHourDiscount%.2f")
      println(f"The service charge is: $currencySymbol$serviceCharge%.2f")
      println("---------")
      println(f"The total for the order is: $currencySymbol$totalCharge%.2f")

      // Additional Information
      println("---------")
      println(s"Transaction time: ${Calendar.getInstance.getTime}") // got local date and time using different method (just for practice sake)
      println(s"Card ending in: ${customer.cardEnding}")
    }
  }

  // List of ordered items
  val order = List(cheeseSandwich, steakSandwich, cola, coffee)

  // Creates a list of premium items in the order to be printed on receipt
  val premiumItemNames = {
    val premiumItems = order.filter(x => (x.isPremium == true))
    premiumItems.map(x => x.name)
  }

  // Creates a list of food items in the order to be printed on receipt
  val foodNames = {
    val foodItems = order.filter(x => (x.isPremium == false) && (x.foodOrDrink == Food))
    foodItems.map(x => x.name)
  }

  // Creates a list of food items in the order to be printed on receipt
  val drinkNames = {
    val drinkNames = order.filter(x => (x.isPremium == false) && (x.foodOrDrink == Drink))
    drinkNames.map(x => x.name)
  }

  // Calling billCalculator method -> pass list of ordered items, customer and currency (GBP, EUR or USD)
  billCalculator(order, bob, GBP)


}
