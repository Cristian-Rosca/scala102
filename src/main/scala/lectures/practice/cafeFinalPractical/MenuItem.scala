package lectures.practice.cafeFinalPractical

case class MenuItem(
                     name: String,
                    foodOrDrink: FoodOrDrink,
                    hotOrCold: HotOrCold,
                    cost: Double,
                    isPremium: Boolean
                   ) //TODO: Perfect naming, especially "isPremium"
