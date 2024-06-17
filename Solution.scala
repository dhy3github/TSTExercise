/**
  Provided case classes
 */
case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(
  cabinCode: String,
  rateCode: String,
  price: BigDecimal,
  rateGroup: String
)
case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

/**
  Promotion Combination solution (Problem 2)

  Invert the given {code}notCombinableWith{/code} relations as combinable and build a map.
  Then, find the maximum cliques in the graph conferred by this map.
 */
class PromoCombos(promos: Seq[Promotion]) {

  val promosByCode: Map[String, Promotion] = promos.map(promo => (promo.code -> promo)).toMap
  val allCodes: Set[String] = promosByCode.keys.toSet
  val allies: Map[String, Set[String]] = promos.map {
    case Promotion(code, foeCodes) => code -> allCodes.diff(foeCodes.toSet)
  }.toMap
  
  lazy val allPromoCombos: Seq[PromotionCombo] = findAllPromoCombos

  private def findAllPromoCombos: Seq[PromotionCombo] = {
    val codeCombos: Set[Set[String]] = allCodes.foldLeft(Set(Set.empty[String])) { (prevCliques, nextCode) =>
      val (ins, outs) = prevCliques.partition(_.forall(allies(_).contains(nextCode)))
      ins.map(_ + nextCode) ++ prevCliques
    }
    val maximumCodeCombos: Set[Set[String]] = codeCombos.filter(combo =>
      codeCombos.forall(superset => combo == superset || !combo.subsetOf(superset))
    )
    maximumCodeCombos.map(_.toSeq).map(PromotionCombo.apply).toSeq
  }

  def getAllPromoCombos: Seq[PromotionCombo] = allPromoCombos
  
  def getPromoCombos(promo: String): Seq[PromotionCombo] = allPromoCombos.filter { combo =>
    combo.promotionCodes contains promo
  }
}

/**
  Group cabin price solution (Problem 1), promotion combination method prototypes as given,
  and all test cases, executable as "main".
 */
object Solution {

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val groupForRateCode: Map[String, String] = rates.map{
      case Rate(code, group) => code -> group
    }.toMap
    val cabinPricesByRateGroup: Map[String, Seq[CabinPrice]] = prices.groupBy(cabinPrice => groupForRateCode(cabinPrice.rateCode))
    val groupedCabinPricesByRateGroup: Map[String, Map[String, Seq[CabinPrice]]] = cabinPricesByRateGroup.map{
      case (group, allCabinPrices) => (group, allCabinPrices.groupBy(_.cabinCode))
    }
    val bestCabinPricesByRateGroup: Map[String, Iterable[CabinPrice]] = groupedCabinPricesByRateGroup.map{
      case (group, cabinPriceGroups) =>
        (group, cabinPriceGroups.map{
          case (cabinCode, cabinPrices) => cabinPrices.minBy(_.price)
        })
    }
    bestCabinPricesByRateGroup.flatMap{
      case (group, bestCabinPrices) =>
        bestCabinPrices.map{
          case(CabinPrice(cabinCode, rateCode, price)) => BestGroupPrice(cabinCode, rateCode, price, group)
        }
    }.toSeq
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    PromoCombos(allPromotions).getAllPromoCombos
    
  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = PromoCombos(allPromotions).getPromoCombos(promotionCode)

  def groupPriceTestCaseGiven: Unit = {
    val rates = Seq(
      Rate("M1", "Military"), Rate("M2", "Military"),
      Rate("S1", "Senior"), Rate("S2", "Senior")
    )
    val cabinPrices = Seq(
      CabinPrice("CA", "M1", 200.00), CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00), CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00), CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00), CabinPrice("CB", "S2", 270.00)
    )
    val expectedBestGroupPrices = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )
    val actual = getBestGroupPrices(rates, cabinPrices)

    assert(expectedBestGroupPrices.forall(actual.contains)
            && actual.length == expectedBestGroupPrices.length)
  }

  def promoComboTestCase(promos: Seq[Promotion], expectedCombos: Seq[PromotionCombo]): Unit = {

    val actual = PromoCombos(promos).getAllPromoCombos

    assert(expectedCombos.forall(combo => actual.exists(_.promotionCodes.toSet == combo.promotionCodes.toSet))
            && expectedCombos.length == actual.length)
  }

  def promoComboTests: Unit = {

    val promosGiven = Seq(
      Promotion("P1", Seq("P3")), Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")), Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    val expectedCombosGiven = Seq(
      PromotionCombo(Seq("P1", "P2")), PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")), PromotionCombo(Seq("P3", "P4", "P5"))
    )
    promoComboTestCase(promosGiven, expectedCombosGiven)

    val promosFullCombo = Seq(
      Promotion("P1", Seq()), Promotion("P2", Seq()),
      Promotion("P3", Seq()), Promotion("P4", Seq())
    )
    val fullCombo = Seq(PromotionCombo(Seq("P1", "P2", "P3", "P4")))
    promoComboTestCase(promosFullCombo, fullCombo)

    val promosIsolateOne = Seq(
      Promotion("P1", Seq("P6")), Promotion("P2", Seq("P6")), Promotion("P3", Seq("P6")),
      Promotion("P4", Seq("P6")), Promotion("P5", Seq("P6")),
      Promotion("P6", Seq("P1", "P2", "P3", "P4", "P5")) 
    )
    val combosIsolateOne = Seq(
      PromotionCombo(Seq("P1", "P2", "P3", "P4", "P5")), PromotionCombo(Seq("P6"))
    )
    promoComboTestCase(promosIsolateOne, combosIsolateOne)

    val promosNoCombo = Seq(
      Promotion("P1", Seq("P2", "P3")), Promotion("P2", Seq("P1", "P3")),
      Promotion("P3", Seq("P1", "P2"))
    )
    val noCombos = Seq(
      PromotionCombo(Seq("P1")), PromotionCombo(Seq("P2")), PromotionCombo(Seq("P3"))
    )
    promoComboTestCase(promosNoCombo, noCombos)
  }

  def main(args: Array[String]) = {
    groupPriceTestCaseGiven
    promoComboTests
  }
}
