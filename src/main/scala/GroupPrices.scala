case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String,
                      rateCode: String,
                      price: BigDecimal)

case class BestGroupPrice(cabinCode: String,
                          rateCode: String,
                          price: BigDecimal,
                          rateGroup: String)

object GroupPrices {

  /*
      The idea here was to build a map of (CabinCode, RateGroup) to a BestGroupPrice
   */
  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val ratesGrouped: Map[String, String] = rateGroups(rates)

    val index = prices.foldLeft(Map[(String, String), BestGroupPrice]()) { (results, price) =>
      ratesGrouped.get(price.rateCode).map { rateGroup =>
        addPriceToResults(results, price, rateGroup)
      }.getOrElse(results)
    }

    index.values.toSeq
  }

  /*
      Building a map of (CabinCode, RateGroup) -> BestGroupPrice
   */
  private def addPriceToResults(results: Map[(String, String), BestGroupPrice],
                        cabinPrice: CabinPrice,
                        rateGroup: String): Map[(String, String), BestGroupPrice] = {
    val key = (cabinPrice.cabinCode, rateGroup)
    results.get(key).map { existingGroupPrice =>
      if (cabinPrice.price < existingGroupPrice.price) {
        results + (key -> BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateGroup))
      } else {
        results
      }
    }.getOrElse {
      results + (key -> BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateGroup))
    }
  }

  /*
    Building an index mapping rate codes to their corresponding rate group
   */
  private def rateGroups(rates: Seq[Rate]): Map[String, String] = {
    rates.foldLeft(Map[String, String]()) { (result, elem) =>
      result + (elem.rateCode -> elem.rateGroup)
    }
  }
}
