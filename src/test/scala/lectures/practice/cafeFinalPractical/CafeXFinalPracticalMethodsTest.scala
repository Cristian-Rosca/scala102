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
import lectures.practice.cafeFinalPractical.CafeXFinalPractical._

import java.time.LocalTime


// Menu
val cola = MenuItem("Cola", Drink, Cold, 0.50, false)
val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Cold, 2.00, false)
val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 4.00, false)
val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)

class CalculateOrderCostTest extends AnyWordSpec {
  "A order cost calculator" should {
    "calculate the total order cost with one item in order list" in {
      val customerOrder = List(cheeseSandwich)
      assert(calculateOrderCost(customerOrder) == 2.0)
    }

    "calculate the total order cost with multiple in order list" in {
      val customerOrder = List(cheeseSandwich, cheeseSandwich, lobster)
      assert(calculateOrderCost(customerOrder) == 29.0)
    }
  }
}

class SetCurrencyMethodTest extends AnyWordSpec {
  "A currency multiplier method" should {
    "calculate the total price of the customer order list and multiply by 1 if currency is GBP" in {
      val currency: Currency = GBP
      val customerOrder = List(cheeseSandwich, cheeseSandwich, lobster)
      assert(calculateOrderCost(customerOrder) * setCurrency(currency) == 29.0)
    }

    "calculate the total price of the customer order list and multiply by 1.15 if currency is EUR" in {
      val currency: Currency = EUR
      val customerOrder = List(cheeseSandwich, cheeseSandwich, lobster)
      assert(calculateOrderCost(customerOrder) * setCurrency(currency) == (29.0 * 1.15))
    }

    "calculate the total price of the customer order list and multiply by 1.23 if currency is USD" in {
      val currency: Currency = USD
      val customerOrder = List(cheeseSandwich, cheeseSandwich, lobster)
      assert(calculateOrderCost(customerOrder) * setCurrency(currency) == (29.0 * 1.23))
    }
  }
}

class getPremiumItemsTest extends AnyWordSpec {
  "A premium items filter" should {
    "Return premium items in customer order" in {
      val customerOrder = List(cheeseSandwich, lobster)
      assert(getPremiumItems(customerOrder) == List(lobster))
    }

    "Return an empty list if there are no premium items in order" in {
      val customerOrder = List(cheeseSandwich)
      assert(getPremiumItems(customerOrder) == List())
    }
  }
}

class getDrinkItemsTest extends AnyWordSpec {
  "A drinks items filter" should {
    "Return drink items in customer order" in {
      val customerOrder = List(coffee, lobster)
      assert(getDrinkItems(customerOrder) == List(coffee))
    }

    "Return an empty list if there are no drink items in order" in {
      val customerOrder = List(cheeseSandwich)
      assert(getDrinkItems(customerOrder) == List())
    }
  }
}

class getHotFoodItemsTest extends AnyWordSpec {
  "A hot food items filter" should {
    "Return hot food in customer order" in {
      val customerOrder = List(steakSandwich, lobster)
      assert(getHotFoodItems(customerOrder) == List(steakSandwich, lobster))
    }

    "Return an empty list if there are no hot food items in order" in {
      val customerOrder = List(coffee)
      assert(getHotFoodItems(customerOrder) == List())
    }
  }
}

class getColdFoodItemsTest extends AnyWordSpec {
  "A cold food items filter" should {
    "Return cold food in customer order" in {
      val customerOrder = List(cheeseSandwich, lobster)
      assert(getColdFoodItems(customerOrder) == List(cheeseSandwich))
    }

    "Return an empty list if there are no cold food items in order" in {
      val customerOrder = List(coffee)
      assert(getColdFoodItems(customerOrder) == List())
    }
  }
}

class getNonPremiumFoodItemsTest extends AnyWordSpec {
  "A non premium food items filter" should {
    "Return non premium foods in customer order" in {
      val customerOrder = List(coffee, lobster, cheeseSandwich)
      assert(getNonPremiumFoodItems(customerOrder) == List(cheeseSandwich))
    }

    "Return an empty list if there are no non premium food items in order" in {
      val customerOrder = List(lobster)
      assert(getNonPremiumFoodItems(customerOrder) == List())
    }
  }
}

class getServiceChargeRatioTest extends AnyWordSpec {
  "A service charge ratio calculator" should {
    "set the tip ratio at 0.25 if there are any premium items present in the customer order" in {
      val customerOrder = List(lobster)
      assert(getServiceChargeRatio(customerOrder) == 0.25)
    }
    "set the tip ratio at 0.20 if there are hot food items present in the customer order" in {
      val customerOrder = List(steakSandwich, cheeseSandwich, coffee)
      assert(getServiceChargeRatio(customerOrder) == 0.2)
    }
    "set the tip ratio at 0.10 if there are cold food items present in the customer order" in {
      val customerOrder = List(cheeseSandwich, coffee)
      assert(getServiceChargeRatio(customerOrder) == 0.1)
    }
    "set the tip ratio at 0 if no food items present in the customer order" in {
      val customerOrder = List(coffee)
      assert(getServiceChargeRatio(customerOrder) == 0)
    }
  }
}

class loyaltyDiscountPercentageMethodTest extends AnyWordSpec {
  "A loyalty discount percentage calculator" should {
    "return 0 if the customer has less than 3 loyalty stars" in {
      val karen = Customer("Karen", 2, 4321)
      assert(loyaltyDiscountPercentage(karen) == 0)
    }
    "return the multiple of loyalty stars and 0.025 if between 3 and 8 stars" in {
      val karen = Customer("Karen", 3, 4321)
      assert(loyaltyDiscountPercentage(karen) == (0.025 * 3))
    }
    "return .2 if the customer has 8 loyalty stars or more" in {
      val karen = Customer("Karen", 9, 4321)
      assert(loyaltyDiscountPercentage(karen) == 0.2)
    }
  }
}

class nonDiscountablePriceCalculator extends AnyWordSpec {
  "A premium item cost calculator" should {
    "return the cost of premium items in a customer order" in {
      val customerOrder = List(lobster, lobster, cola)
      assert(calculateOrderCost(getPremiumItems(customerOrder)) == 50.0)
    }
    "return 0 if there are no premium items in a customer order" in {
      val customerOrder = List(cola)
      assert(calculateOrderCost(getPremiumItems(customerOrder)) == 0)
    }
  }
}

class loyaltyDiscountCalculator extends AnyWordSpec {
  "A loyalty discount calculator" should {
    "return 0 if the customer has less than 3 loyalty stars" in {
      val karen = Customer("Karen", 0, 4321)
      val customerOrder = List(steakSandwich, steakSandwich, cheeseSandwich)
      val currency = GBP
      val price = calculateOrderCost(customerOrder) * setCurrency(currency)
      val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency)
      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage(karen) == 0)
    }
    "return 15% of the price of non-Premium if the customer has 6 loyalty stars" in {
      val karen = Customer("Karen", 6, 4321)
      val customerOrder = List(steakSandwich, steakSandwich, cheeseSandwich)
      val currency = GBP
      val price = calculateOrderCost(customerOrder) * setCurrency(currency)
      val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency)
      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage(karen) == 1.5000000000000002)
    }
    "return 20% of the price of non-Premium if the customer has 10 loyalty stars" in {
      val karen = Customer("Karen", 10, 4321)
      val customerOrder = List(steakSandwich, steakSandwich, cheeseSandwich)
      val currency = GBP
      val price = calculateOrderCost(customerOrder) * setCurrency(currency)
      val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency)
      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage(karen) == 2.0)
    }
    "return 0 if all the items in the order are premium regardless of customer stars" in {
      val karen = Customer("Karen", 10, 4321)
      val customerOrder = List(lobster, lobster, lobster)
      val currency = GBP
      val price = calculateOrderCost(customerOrder) * setCurrency(currency)
      val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency)
      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage(karen) == 0)
    }
    "exclude the price of premium items from the discount calculation" in {
      val karen = Customer("Karen", 10, 4321)
      val customerOrder = List(steakSandwich, steakSandwich, cheeseSandwich, lobster)
      val currency = GBP
      val price = calculateOrderCost(customerOrder) * setCurrency(currency)
      val nonDiscountablePrice = calculateOrderCost(getPremiumItems(customerOrder)) * setCurrency(currency)
      assert((price - nonDiscountablePrice) * loyaltyDiscountPercentage(karen) == 2.0)
    }
  }
}