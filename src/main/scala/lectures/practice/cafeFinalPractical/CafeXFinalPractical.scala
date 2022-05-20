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
  val cola = MenuItem(name ="Cola",foodOrDrink = Drink,hotOrCold = Cold, cost = 0.50, isPremium = false)
  val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
  val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
  val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 4.00, false)
  val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)

  //Customers
  val karen = Customer("Karen", 10, 4321)
  val bob = Customer("Bob", 5, 2001)


  // calculate cost of order
  def calculateOrderCost(customerOrder: List[MenuItem]): Double = customerOrder.map(x => x.cost).sum

  // get loyalty discount percentage
  def calculateLoyaltyDiscountPercentage(customer: Customer): Double = {
    if (customer.loyaltyStars < 3) 0
    else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
    else 0.2
  }


  def getPremiumItemsFromOrder(customerOrder: List[MenuItem]): List[MenuItem] = customerOrder.filter(x => x.isPremium == true)

  def getDrinkItemsFromOrder(customerOrder: List[MenuItem]): List[MenuItem] = customerOrder.filter(x => x.foodOrDrink == Drink)

  def getHotFoodItemsFromOrder(customerOrder: List[MenuItem]): List[MenuItem] = customerOrder.filter(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food))

  def getColdFoodItemsFromOrder(customerOrder: List[MenuItem]): List[MenuItem] = customerOrder.filter(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food))

  def getNonPremiumFoodItemsFromOrder(customerOrder: List[MenuItem]): List[MenuItem] = customerOrder.filter(x => (x.isPremium == false) && (x.foodOrDrink == Food))


  def getServiceChargeRatio(customerOrder: List[MenuItem]): Double = {
    if (getPremiumItemsFromOrder(customerOrder).nonEmpty) 0.25 // 25% service charge if premium food in order
    else if (getHotFoodItemsFromOrder(customerOrder).nonEmpty) 0.20 // 20% service charge if hot food in order
    else if (getColdFoodItemsFromOrder(customerOrder).nonEmpty) 0.1 // 10% service charge if food in order
    else 0 // No service charge if no items are foods
  }

  def getPremiumItemNames(customerOrder: List[MenuItem]): List[String] = {
    getPremiumItemsFromOrder(customerOrder).map(x => x.name)
  }

  def getNonPremiumFoodItemNames(customerOrder: List[MenuItem]): List[String] = {
    getNonPremiumFoodItemsFromOrder(customerOrder).map(x => x.name)
  }

  def getDrinkNames(customerOrder: List[MenuItem]): List[String] = {
    getDrinkItemsFromOrder(customerOrder).map(x => x.name)
  }

  val timeNow: Int = LocalTime.now.getHour()

  def isHappyHour: Boolean = if ((timeNow > 17) && (timeNow < 22)) true else false


  def generateCustomerBill(customerOrder: List[MenuItem], customer: Customer, selectedCurrency: Currency) = {
    if (customerOrder.isEmpty) println("No items added to customer order. Please add items and try again") else {

      val chosenCurrency: (Double, String) =
        selectedCurrency match {
          case GBP => (1, "£")
          case EUR => (1.15, "")
          case USD => (1.23, "")
          case _ => (1, "")
        }

      val totalOrderItemsCost: Double = calculateOrderCost(customerOrder) * chosenCurrency._1

      val nonDiscountablePrice = calculateOrderCost(getPremiumItemsFromOrder(customerOrder)) * chosenCurrency._1

      val loyaltyDiscount: Double = (totalOrderItemsCost - nonDiscountablePrice) * calculateLoyaltyDiscountPercentage(customer)

      val drinksOrderCost: Double = calculateOrderCost(getDrinkItemsFromOrder(customerOrder)) * chosenCurrency._1
      
      val happyHourDiscount: Double = {
        if (isHappyHour) drinksOrderCost * 0.5
        else 0
      }

      val orderCostAfterDiscounts: Double = totalOrderItemsCost - loyaltyDiscount - happyHourDiscount


      val serviceCharge: Double = {
        val serviceChargeRatio: Double = getServiceChargeRatio(customerOrder)

        if (getPremiumItemsFromOrder(customerOrder).nonEmpty)
          if (orderCostAfterDiscounts * serviceChargeRatio >= 40) 40 // Maximum of £40 if premium items in order
          else orderCostAfterDiscounts * serviceChargeRatio
        else if (getHotFoodItemsFromOrder(customerOrder).nonEmpty)
          if (orderCostAfterDiscounts * serviceChargeRatio >= 20) 20 // Maximum of £20 if hot food items in order
          else orderCostAfterDiscounts * serviceChargeRatio
        else orderCostAfterDiscounts * serviceChargeRatio
      }

      val recommendedTotalCharge: Double = orderCostAfterDiscounts + serviceCharge



      // Statements to print customer bill
      println(s"Thank you for shopping with us, ${customer.name}!")

      println("Please see your order summary below")
      println("---------")
      println("Premium Items")
      for (element <- getPremiumItemNames(customerOrder)) {
        println(s"• ${element}")
      }

      println("---------")
      println("Food")
      for (element <- getNonPremiumFoodItemNames(customerOrder)) {
        println(s"• ${element}")
      }

      println("---------")
      println("Drinks")
      for (element <- getDrinkNames(customerOrder)) {
        println(s"• ${element}")
      }

      println("---------")
      println(f"The cost of your items is: ${chosenCurrency._2}$totalOrderItemsCost%.2f")
      println(f"Based on ${customer.loyaltyStars} loyalty stars, you received a loyalty discount of: ${chosenCurrency._2}$loyaltyDiscount%.2f")
      if ((isHappyHour)) println(f"The happy hour discount is: ${chosenCurrency._2}$happyHourDiscount%.2f")
      println(f"The service charge is: ${chosenCurrency._2}$serviceCharge%.2f")
      println("---------")
      println(f"The total for the order is: ${chosenCurrency._2}$recommendedTotalCharge%.2f")

      println("---------")
      println(s"Transaction time: ${Calendar.getInstance.getTime}")
      println(s"Card ending in: ${customer.cardEnding}")
    }
  }


  // List of ordered items
  val customerOrder = List(cheeseSandwich, coffee, steakSandwich)

  generateCustomerBill(customerOrder = customerOrder,customer = bob,selectedCurrency = GBP)

}
