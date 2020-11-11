object Driver {
  def main(args: Array[String]): Unit = {

    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    println("Best Group Prices:")
    GroupPrices.getBestGroupPrices(rates, prices).foreach(println)
    println("")

    val promos = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    println("All promotion combinations:")
    PromotionCombos.allCombinablePromotions(promos).foreach(println)
    println("")
    println("All promotion combinations for promo code P1:")
    PromotionCombos.combinablePromotions("P1", promos).foreach(println)
    println("")
    println("All promotion combinations for promo code P3:")
    PromotionCombos.combinablePromotions("P3", promos).foreach(println)
  }
}
