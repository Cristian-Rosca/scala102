package lectures.practice.cafeFinalPractical

import org.scalatest.wordspec.AnyWordSpec
import CafeXFinalPractical.{timeNow, *}
import lectures.practice.cafeFinalPractical.Currency.GBP
import lectures.practice.cafeFinalPractical.Currency.EUR
import lectures.practice.cafeFinalPractical.Currency.USD
import lectures.practice.cafeFinalPractical.FoodOrDrink.*
import lectures.practice.cafeFinalPractical.HotOrCold.*
import lectures.practice.cafeFinalPractical.MenuItem
import lectures.practice.cafeFinalPractical.Customer

import java.time.LocalTime




// Menu
val cola = MenuItem("Cola", Drink, Cold, 0.50, false)
val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 2.00, false)
val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)



  class PriceCalculatorTest extends AnyWordSpec {

    "Price calculator function" should {
      "calculate the total price of the customer order list with one item" in {
        val customerOrder = List(cheeseSandwich)
        assert((customerOrder.map(x => x.cost).sum) == 2.0)
      }
    }

    "Price calculator function" should {
      "calculate the total price of the customer order list with multiple items" in {
        val customerOrder = List(cheeseSandwich, lobster)
        assert((customerOrder.map(x => x.cost).sum) == 27.0)
      }
    }
  }


class CurrencyMultiplierTest extends AnyWordSpec {
  "Price calculator function with currency multiplier" should {
    "calculate the total price of the customer order list and multiply by 1 if currency is GBP" in {
      val currency: Currency = GBP
      val currencyMultiplier = currency match {
        case GBP => 1
        case EUR => 1.15
        case USD => 1.23
        case _ => 1
      }
      val customerOrder = List(cheeseSandwich)
      assert((customerOrder.map(x => x.cost).sum * currencyMultiplier) == 2.0)
    }

    "calculate the total price of the customer order list and multiply by 1.15 if currency is EUR" in {
      val currency: Currency = EUR
      val currencyMultiplier = currency match {
        case GBP => 1
        case EUR => 1.15
        case USD => 1.23
        case _ => 1
      }
      val customerOrder = List(cheeseSandwich)
      assert((customerOrder.map(x => x.cost).sum * currencyMultiplier) == (2.0 * 1.15))
    }

    "calculate the total price of the customer order list and multiply by 1.23 if currency is USD" in {
      val currency: Currency = USD
      val currencyMultiplier = currency match {
        case GBP => 1
        case EUR => 1.15
        case USD => 1.23
        case _ => 1
      }
      val customerOrder = List(cheeseSandwich)
      assert((customerOrder.map(x => x.cost).sum * currencyMultiplier) == (2.0 * 1.23))
    }


  }
}

class PremiumItemFilterTest extends AnyWordSpec {
  "Premium Item Filter" should {
    "Return premium items in customer order" in {
      val customerOrder = List(cheeseSandwich, lobster)
      assert(customerOrder.filter(x => (x.isPremium == true)) == List(lobster))
      assert(customerOrder.filter(x => (x.isPremium == true)) != List(cheeseSandwich))
    }

    "Return an empty list if there are no premium items in order" in {
      val customerOrder = List(cheeseSandwich)
      assert(customerOrder.filter(x => (x.isPremium == true)) == List())
    }
  }
}



class LoyaltyPercentageCalculatorTest extends AnyWordSpec {
  "Loyalty Calculator" should {
    "not give any loyalty discount to any customer with less than 3 stars" in {
      val customer = Customer("Karen", 2, 4321)

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      assert(loyaltyDiscountPercentage == 0)
    }

      "give a 0.025 discount for each star the customer has up to 8 stars" in {
        val customer = Customer("Karen", 6, 4321)

        val loyaltyDiscountPercentage =
          if (customer.loyaltyStars < 3) 0
          else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
          else 0.2

        assert(loyaltyDiscountPercentage == 0.15000000000000002)
      }
    }

  "cap the discount percentage to a maximum of .2 once a customer has 8 loyalty stars or more" in {
    val customer = Customer("Karen", 9, 4321)

    val loyaltyDiscountPercentage =
      if (customer.loyaltyStars < 3) 0
      else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
      else 0.2

    assert(loyaltyDiscountPercentage == 0.2)
  }
}

class PriceMinusLoyaltyDiscountTest extends AnyWordSpec {
  "Loyalty Discount Calculator" should {
    "calculate the loyalty discount if there are no premium items and no loyalty stars" in {
      val customer = Customer("Karen", 2, 4321)
      val customerOrder = List(cheeseSandwich)

      val currencyMultiplier = 1

      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage == 0)
    }

    "calculate the loyalty discount if there are no premium items and less than 8 loyalty stars" in {
      val customer = Customer("Karen", 6, 4321)
      val customerOrder = List(cheeseSandwich)

      val currencyMultiplier = 1

      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage == 0.30000000000000004)
    }

    "calculate the loyalty discount if there are no premium items and there are 8 stars or more" in {
      val customer = Customer("Karen", 8, 4321)
      val customerOrder = List(cheeseSandwich)

      val currencyMultiplier = 1

      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage == 0.4)
    }

    "calculate the loyalty discount if there are premium items and no loyalty stars" in {
      val customer = Customer("Karen", 2, 4321)
      val customerOrder = List(cheeseSandwich, lobster, lobster)

      val currencyMultiplier = 1

      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage == 0)
    }

    "calculate the loyalty discount if there are premium items and less than 8 loyalty stars" in {
      val customer = Customer("Karen", 6, 4321)
      val customerOrder = List(cheeseSandwich, lobster, lobster)

      val currencyMultiplier = 1

      val price = customerOrder.map(x => x.cost).sum * currencyMultiplier

      val loyaltyDiscountPercentage =
        if (customer.loyaltyStars < 3) 0
        else if (customer.loyaltyStars < 9) customer.loyaltyStars * 0.025
        else 0.2

      val premiumItemsList = customerOrder.filter(x => (x.isPremium == true))
      val nonDiscountablePrice = premiumItemsList.map(x => x.cost).sum

      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage == 0.30000000000000004)
    }
  }
}

class DrinksCostCalculatorFunctionTest extends AnyWordSpec {
  "A drink cost calculator function" should {
    "return the sum of the cost of drinks from the customer order" in {
      val customerOrder = List(coffee, coffee, coffee, lobster, cheeseSandwich, steakSandwich)
      assert(customerOrder.filter(x => (x.foodOrDrink == Drink)).map(x => x.cost).sum == 3.0)
    }

    "return 0 if there are no drinks in the customer order" in {
      val customerOrder = List(lobster, cheeseSandwich, steakSandwich)
      assert(customerOrder.filter(x => (x.foodOrDrink == Drink)).map(x => x.cost).sum == 0)
    }
  }
}

class HappyHourDiscountCalculatorTest extends AnyWordSpec {
  "A happy hour discount calculator" should {
    "return half of the total price of drinks if the local time in hours is between two specified times" in {
      val customerOrder = List(coffee, coffee, coffee)
      val drinksOrderCost = customerOrder.filter(x => (x.foodOrDrink == Drink)).map(x => x.cost).sum
      // Get local time in hours
      val timeNow = LocalTime.now.getHour()
      val happyHourDiscount = if ((timeNow > 11) && (timeNow < 22)) drinksOrderCost * 0.5 else 0
      assert(happyHourDiscount == 1.5)
    }

    "will return 0 if local time is not between specified times" in {
      val customerOrder = List(coffee, coffee, coffee)
      val drinksOrderCost = customerOrder.filter(x => (x.foodOrDrink == Drink)).map(x => x.cost).sum
      // Get local time in hours
      val timeNow = LocalTime.now.getHour()
      val happyHourDiscount = if ((timeNow > 21) && (timeNow < 22)) drinksOrderCost * 0.5 else 0
      assert(happyHourDiscount == 0)
    }
  }
}

class TipMultiplierCalculatorTest extends AnyWordSpec {
  "A tip multiplier ratio calculator" should {
    "set the tip ratio at 0.25 if there are any premium items present in the customer order" in {
      val customerOrder = List(coffee, coffee, coffee, steakSandwich, lobster)
      val tipMultiplier: Double = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 25% service charge if premium food in order
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
        else if (customerOrder.find(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food)).isDefined) 0.1 // 10% service charge if food in order
        else 0 // No service charge if no items are foods
      }
      assert(tipMultiplier == 0.25)
    }

    "set the tip ratio at 0.2 if there hot food items in the customer order but no premium items" in {
      val customerOrder = List(coffee, coffee, coffee, steakSandwich)
      val tipMultiplier: Double = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 25% service charge if premium food in order
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
        else if (customerOrder.find(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food)).isDefined) 0.1 // 10% service charge if food in order
        else 0 // No service charge if no items are foods
      }
      assert(tipMultiplier == 0.20)
    }

    "set the tip ratio at 0.1 if there food items in the customer order but no hot food items and no premium items" in {
      val customerOrder = List(coffee, coffee, coffee, cheeseSandwich)
      val tipMultiplier: Double = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 25% service charge if premium food in order
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
        else if (customerOrder.find(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food)).isDefined) 0.1 // 10% service charge if food in order
        else 0 // No service charge if no items are foods
      }
      assert(tipMultiplier == 0.10)
    }

    "set the tip ratio at 0 if there are only drinks in the order" in {
      val customerOrder = List(coffee, coffee, coffee)
      val tipMultiplier: Double = {
        if (customerOrder.find(x => (x.isPremium == true) && (x.foodOrDrink == Food)).isDefined) 0.25 // 25% service charge if premium food in order
        else if (customerOrder.find(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food)).isDefined) 0.20 // 20% service charge if hot food in order
        else if (customerOrder.find(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food)).isDefined) 0.1 // 10% service charge if food in order
        else 0 // No service charge if no items are foods
      }
      assert(tipMultiplier == 0)
    }
  }
}

class ServiceChargeCalculatorTest extends AnyWordSpec {
  "A service charge calculator" should {
    "calculate the service charge at 25% on an order containing a premium item" in {
      val finalPrice = 26.0
      val customerOrder = List(lobster, coffee)

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

      assert(serviceCharge == 6.5)
    }

    "cap service charge to a maximum of 40 on orders containing premium items" in {
      val finalPrice = 200
      val customerOrder = List(lobster, lobster, lobster, lobster, lobster, lobster, lobster, lobster)

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

      assert(serviceCharge == 40)
    }

    "calculate the service charge at 20% on an order containing hot food items" in {
      val finalPrice = 5.0
      val customerOrder = List(steakSandwich, coffee)

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

      assert(serviceCharge == 1.0)
    }
    "cap service charge to a maximum of 20 on orders containing hot food items" in {
      val finalPrice = 200
      val customerOrder = List(steakSandwich)

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

      assert(serviceCharge == 20)
    }
    "calculate the service charge at 10% on an order containing a cold food item" in {
      val finalPrice = 10
      val customerOrder = List(cheeseSandwich)

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

      assert(serviceCharge == 1.0)
    }
    "calculate the service charge at 10% on an order containing a cold food item without any maximum cap" in {
      val finalPrice = 100000
      val customerOrder = List(cheeseSandwich)

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

      assert(serviceCharge == 10000.0)
    }
    "calculate service charge at 0 if there are only drink items in the order" in {
      val finalPrice = 100000
      val customerOrder = List(coffee, coffee, cola)

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

      assert(serviceCharge == 0)
    }
  }


}







