import org.scalatest._
import flatspec._
import matchers._

class PromoComboSpec extends AnyFlatSpec {
  val P1 = "P1"
  val P2 = "P2"
  val P3 = "P3"
  val P4 = "P4"
  val P5 = "P5"

  val promos = Seq(
    Promotion(P1, Seq(P3)),
    Promotion(P2, Seq(P4, P5)),
    Promotion(P3, Seq(P1)),
    Promotion(P4, Seq(P2)),
    Promotion(P5, Seq(P2))
  )

  val allComboResults = Seq(
    PromotionCombo(Seq(P1, P2)),
    PromotionCombo(Seq(P1, P4, P5)),
    PromotionCombo(Seq(P2, P3)),
    PromotionCombo(Seq(P3, P4, P5))
  )

  val resultsForP1 = Seq(
    PromotionCombo(Seq(P1, P2)),
    PromotionCombo(Seq(P1, P4, P5))
  )

  val resultsForP3 = Seq(
    PromotionCombo(Seq(P3, P2)),
    PromotionCombo(Seq(P3, P4, P5))
  )

  def equals(one: Seq[PromotionCombo], two: Seq[PromotionCombo]): Boolean = {
    val set1 = convertToSet(one)
    val set2 = convertToSet(two)
    set1 == set2
  }

  def convertToSet(combos: Seq[PromotionCombo]): Set[Set[String]] = {
    combos.map { combo =>
      combo.promotionCodes.toSet
    }.toSet
  }

  "PromotionCombos" should "be correctly generated from a list of promos" in {
    assert(equals(PromotionCombos.allCombinablePromotions(promos), allComboResults))
  }

  it should "correctly find promos for P1" in {
    assert(equals(PromotionCombos.combinablePromotions(P1, promos), resultsForP1))
  }

  it should "correctly find promos for P3" in {
    assert(equals(PromotionCombos.combinablePromotions(P3, promos), resultsForP3))
  }
}
