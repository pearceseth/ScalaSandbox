case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object PromotionCombos {
  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val data = generateCombinations(allPromotions)
    data.map(set => PromotionCombo(set.toSeq)).toSeq
  }

  /*
    Idea here is to represent a a promotion as a vector where a column is 1 if that promo
    can be combined with the promo for that index, and zero if not.

    So from the examples given, the matrix would look like:
    "P1" -> [1,1,0,1,1]
    "P2" -> [1,1,1,0,0]
    "P3" -> [0,1,1,1,1]
    "P4" -> [1,0,1,1,1]
    "P5" -> [1,0,1,1,1]

    For there, we can build sets of combinable promos by multiplying vectors. This gives a Set of sets of promos.
    For example, P1 X P2 gives the vector [1,1,0,0,0] which can be translated into [P1, P2], one of the valid
    results.

    To make this work, we can't combine vectors that ar identical or vectors that are invalid combos based on
    "notCombinableWith". For example, don't combine P1 & P3 (which are invalid to combine), or P4 & P5 (which
    are identical).

    This method produces some duplicate results so I'm using Sets to eliminate them.

    I was originally planning on a graph based algorithm, but in the interest of time I used this method since
    it seemed easier to write. If you build a graph based on the promo codes, then the set of valid promo combos
    can be generated from different valid paths through the graph.
   */
  private def generateCombinations(promos: Seq[Promotion]): Set[Set[String]] = {
    val keys = promos.map(_.code).toArray
    val matrix = buildMatrix(promos)

    promos.foldLeft(Set[Set[String]]()) { (data, promo) =>
      matrix.get(promo.code).map { arrayForPromo =>
        val blacklist = Seq(promo.code) ++ promo.notCombinableWith
        val input = matrix.filter(row => !blacklist.contains(row._1)).values.toSeq

        input.filter(!areEqual(_, arrayForPromo)).map { promoToCompare =>
          val scores = combine(arrayForPromo, promoToCompare)
          toPromoSet(scores, keys)
        }
      }.map { set =>
        data ++ set
      }.getOrElse(data)
    }
  }

  private def buildMatrix(promos: Seq[Promotion]): Map[String, Array[Int]] = {
    val keys = promos.map(_.code)
    promos.map { promo =>
      val initial = Array.fill(promos.size)(1)
      val arrayData = promo.notCombinableWith.foldLeft(initial) { (results, element) =>
        val index = keys.indexOf(element)
        results(index) = 0
        results
      }
      promo.code -> arrayData
    }.toMap
  }

  /*
    1-D vector multiplication
   */
  private def combine(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    array1.zip(array2).map { case (x, y) =>
      if (x == 0 || y == 0) 0 else 1
    }
  }

  /*
    Turn an array representing to combine vectors into a set of promo codes.
    If the element is 1, include the corresponding promo code in the result set
   */
  private def toPromoSet(array: Array[Int], keys: Array[String]): Set[String] = {
    array.zipWithIndex.foldLeft(Set[String]()) { (set, data) =>
      if (data._1 != 0) set + keys(data._2) else set
    }
  }

  private def areEqual(array1: Array[Int], array2: Array[Int]): Boolean = {
    array1.zip(array2).forall { case (left, right) =>  left == right }
  }
}
