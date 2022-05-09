package lectures.practice.cafeFinalPractical

import org.scalatest.wordspec.AnyWordSpec

import CafeXFinalPractical._
import lectures.practice.cafeFinalPractical.Currency.GBP
import lectures.practice.cafeFinalPractical.Currency.EUR
import lectures.practice.cafeFinalPractical.Currency.USD
import lectures.practice.cafeFinalPractical.FoodOrDrink.*
import lectures.practice.cafeFinalPractical.HotOrCold.*
import lectures.practice.cafeFinalPractical.MenuItem
import lectures.practice.cafeFinalPractical.Customer




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
    "not given any loyalty discount to any customer with less than 3 stars" in {
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










