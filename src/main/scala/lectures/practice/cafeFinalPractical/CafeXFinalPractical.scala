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
  //TODO: First off, really happy with how the case classes, objects and traits were used!
  // Menu
  val cola = MenuItem("Cola", Drink, Cold, 0.50, false) //TODO: using named parameters helps us to recognise what these types correlate to, e.g. MenuItem(..., isPremium = false)
  val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
  val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
  val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 4.00, false)
  val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)

  //Customers
  val karen = Customer("Karen", 10, 4321) //TODO: again, named parameters
  val bob = Customer("Bob", 5, 2001)

  //TODO: So we don't tend to comment on what a method does, the name of the method and the contents should really explain it, if it doesn't then you've written it wrong!
  // set currency for calculations
  // Update exchange rates here if necessary - can import live fx rates as an extension
  def setCurrency(currency: Currency) = { //TODO: Return type?
    currency match { //TODO: Great use of pattern matching!
      case GBP => 1
      case EUR => 1.15
      case USD => 1.23
      case _ => 1
    }
  } //TODO: setCurrency sounds like you should be returning a currency, but we are returning a value... possibly getCurrencyValue or something?

  // Changes currency symbol in receipt
  def setCurrencySymbol(currency: Currency) = { //TODO: could the above function be combined into this one???
    currency match {
      case GBP => "£"
      case EUR => "€"
      case USD => "$"
      case _ => "£"
    }
  }

  //TODO: again, these comments aren't needed
  // calculate cost of order
  def calculateOrderCost(customerOrder: List[MenuItem]) = customerOrder.map(x => x.cost).sum //TODO: return type?

  // get loyalty discount percentage
  def loyaltyDiscountPercentage(customer: Customer) = { //TODO: HUGE NO NO! DON'T RETURN AN "ANY" TYPE!!! (Can you spot why this is an Any Type? Hint: Try giving this a return type now)
    if (customer.loyaltyStars < 3) 0
    else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
    else 0.2
  }

  //TODO: These 5 functions could be combined into one function????
  // get premium items method
  def getPremiumItems(customerOrder: List[MenuItem]) = customerOrder.filter(x => (x.isPremium == true)) //TODO: don't need hte == true

  // get drink items method
  def getDrinkItems(customerOrder: List[MenuItem]) = customerOrder.filter(x => (x.foodOrDrink == Drink)) //TODO: we tend to use .equals over ==

  // get hot food items
  def getHotFoodItems(customerOrder: List[MenuItem]) = customerOrder.filter(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)) //TODO: Good use of filter throughout

  // get cold food items
  def getColdFoodItems(customerOrder: List[MenuItem]) = customerOrder.filter(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food))

  // get non premium foods
  def getNonPremiumFoodItems(customerOrder: List[MenuItem]) = customerOrder.filter(x => (x.isPremium == false) && (x.foodOrDrink == Food))

  // get service charge ratio based on items in customer order
  def getServiceChargeRatio(customerOrder: List[MenuItem]) = { //TODO: Great use of .nonEmpty, but wasn't there a cap up to £20?
    if (getPremiumItems(customerOrder).nonEmpty) 0.25 // 25% service charge if premium food in order
    else if (getHotFoodItems(customerOrder).nonEmpty) 0.20 // 20% service charge if hot food in order
    else if (getColdFoodItems(customerOrder).nonEmpty) 0.1 // 10% service charge if food in order
    else 0 // No service charge if no items are foods
  }

  // Creates a list of premium items in the order to be printed on receipt
  def getPremiumItemNames(customerOrder: List[MenuItem]) = { //TODO: RETURNING AN ANY!!!
    getPremiumItems(customerOrder).map(x => x.name) //TODO: Try to avoid using x in your maps and filters, a simple name will add to your readability
  }

  // Creates a list of food items in the order to be printed on receipt
  def getNonPremiumFoodItemNames(customerOrder: List[MenuItem]) = {
    getNonPremiumFoodItems(customerOrder).map(x => x.name)
  }

  // get list of drink names to be printed on receipt
  def getDrinkNames(customerOrder: List[MenuItem]) = { //TODO: could the three methods above be combined???
    getDrinkItems(customerOrder).map(x => x.name)
  }

  // Get local time in hours
  val timeNow = LocalTime.now.getHour()

  // Check if happy hour is on now
  def isHappyHour: Boolean = if ((timeNow > 17) && (timeNow < 22)) true else false //TODO: What does ((timeNow > 17) && (timeNow < 22)) return?


  // BILL CALCULATOR
  // Takes a list of ordered items and the customer
  def billCalculator(customerOrder: List[MenuItem], customer: Customer, currency: Currency) = {
    if (customerOrder.isEmpty) println("No items added to customer order. Please add items and try again") else { //TODO: Good spot for handling a possible error, would an error class be possible here?


      // Sum of cost of all items in order list (adjusted based on selected currency)
      val price = calculateOrderCost(customerOrder) * setCurrency(currency) //TODO: the return type of price is Any...
      //TODO: Instead of price, what about inital price (I'm being picky here)

      // Calculates the loyalty discount for the customer
      val loyaltyDiscount = {
        // Premium Items are not discountable - get cost of premium items in order
        val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency) //TODO: A val inside another val is not great
        // calculate the total loyalty discount the customer gets
        (price - nonDiscountablePrice) * loyaltyDiscountPercentage(customer)
      }


      val happyHourDiscount = {
        // Get total cost of drinks in order
        val drinksOrderCost = calculateOrderCost(getDrinkItems(customerOrder)) * setCurrency(currency)

        // If local hours is between 18 and 21 happyHourDiscount is half of drink prices, else it is 0
        if (isHappyHour) drinksOrderCost * 0.5
        else 0
      }

      // Full order price - Loyalty discount - happy hour discount (if applicable)
      val finalPrice = price - loyaltyDiscount - happyHourDiscount


      // Service Charge
      // Calculated based on finalPrice (Full order price - Loyalty discount)
      val serviceCharge: Double = {
        val serviceChargeRatio = getServiceChargeRatio(customerOrder)

        if (getPremiumItems(customerOrder).nonEmpty)
          if (finalPrice * serviceChargeRatio >= 40) 40 // Maximum of £40 if premium items in order
          else finalPrice * serviceChargeRatio
        else if (getHotFoodItems(customerOrder).nonEmpty)
          if (finalPrice * serviceChargeRatio >= 20) 20 // Maximum of £20 if hot food items in order
          else finalPrice * serviceChargeRatio
        else finalPrice * serviceChargeRatio
      }

      // calculates total charge
      val totalCharge = finalPrice + serviceCharge



      // Statements to print customer bill
      println(s"Thank you for shopping with us, ${customer.name}!")

      // Print names of premium items ordered
      println("Please see your order summary below")
      println("---------")
      println("Premium Items")
      for (element <- getPremiumItemNames(customerOrder)) {
        println(s"• ${element}")
      }

      // Print names of food items ordered
      println("---------")
      println("Food")
      for (element <- getNonPremiumFoodItemNames(customerOrder)) {
        println(s"• ${element}")
      }

      // Print names of drinks ordered
      println("---------")
      println("Drinks")
      for (element <- getDrinkNames(customerOrder)) {
        println(s"• ${element}")
      }

      println("---------")
      println(f"The cost of your items is: ${setCurrencySymbol(currency)}$price%.2f")
      println(f"Based on ${customer.loyaltyStars} loyalty stars, you received a loyalty discount of: ${setCurrencySymbol(currency)}$loyaltyDiscount%.2f")
      if ((isHappyHour)) println(f"The happy hour discount is: ${setCurrencySymbol(currency)}$happyHourDiscount%.2f")
      println(f"The service charge is: ${setCurrencySymbol(currency)}$serviceCharge%.2f")
      println("---------")
      println(f"The total for the order is: ${setCurrencySymbol(currency)}$totalCharge%.2f")

      // Additional Information
      println("---------")
      println(s"Transaction time: ${Calendar.getInstance.getTime}") // got local date and time using different method (just for practice sake)
      println(s"Card ending in: ${customer.cardEnding}")
    }
  }


  // List of ordered items
  val order = List(cheeseSandwich, coffee, steakSandwich)

  // Calling billCalculator method -> pass list of ordered items, customer and currency (GBP, EUR or USD)
  billCalculator(order, bob, GBP)


}
