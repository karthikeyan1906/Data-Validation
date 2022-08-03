import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object PriceCalculator extends App{

  var sampleData = getSampleData

  var results = calculatePrices(sampleData)
  results.foreach(r => {
    printf("Force price for product - %s : %s\n", r.getProductID, r.getForcePrice)
    printf("GuardRail price for product - %s : %s\n", r.getProductID, r.getGuardRailPrice)
    printf("Rounded price for product - %s : %s\n", r.getProductID, r.getRoundedPrice)
    printf("Category Price for product - %s : %s\n", r.getProductID, r.getCategoryPrice)
    printf("Matrix Percentage for product - %s : %s\n", r.getProductID, r.getMatrixPercentage)
  })

  def calculatePrices(productMetrics: List[ProductMetric]): ListBuffer[ProductPrices] = {
    @tailrec
    def initialCalculation(products: List[ProductMetric], agg: ListBuffer[ProductPrices]): ListBuffer[ProductPrices] = {
      val calculate = PriceCalculator(products.head)

      val productPrice: ProductPrices = calculate productPrices

      val forcePriceReceived:Double = calculate.forcePrice
      productPrice.setForcePrice(forcePriceReceived)

      val guardRailPrice: Double = calculate guardRailPrice forcePriceReceived
      productPrice.setGuardRailPrice(guardRailPrice)

      val roundedPrice: Double = calculate roundedPrice guardRailPrice
      productPrice.setRoundedPrice(roundedPrice)

      agg.append(productPrice)
      if (products.tail.isEmpty) agg
      else initialCalculation(products.tail, agg)
    }

    val preCalculatedPrices = initialCalculation(productMetrics, ListBuffer[ProductPrices]())

    @tailrec
    def postCalculationOnPrices(preCalcPrices: ListBuffer[ProductPrices]): Unit = {
      val calculate = PriceCalculator()

      val categoryPrice = calculate categoryPrice(preCalcPrices.head, preCalcPrices)
      preCalcPrices.head.setCategoryPrice(categoryPrice)

      val matrixPercentage = calculate matrixPercentage(preCalcPrices.head, preCalcPrices)
      preCalcPrices.head.setMatrixPercentage(matrixPercentage)

      if (preCalcPrices.tail.isEmpty) Unit
      else postCalculationOnPrices(preCalcPrices.tail)
    }
    postCalculationOnPrices(preCalculatedPrices)

    val calculator = PriceCalculator()
    calculator aggregatedRoundedPrice preCalculatedPrices

    preCalculatedPrices
  }

  def getSampleData:List[ProductMetric] = {
    val product1Competitor1 = new Competitor
    product1Competitor1.setName("1")
    product1Competitor1.setPrice(23.4)
    product1Competitor1.setWeight(50)

    val product1Competitor2 = new Competitor
    product1Competitor2.setName("2")
    product1Competitor2.setPrice(100)
    product1Competitor2.setWeight(50)

    val product1 = new ProductMetric
    product1.setProductID("id1")
    product1.setMatrixID("matrix2")
    product1.setLocationLevel2("level2")
    product1.setPsku("Product1")
    product1.setCategoryID("1")
    product1.setVolume(3000)
    product1.setLowerGuardrail(20.0)
    product1.setUpperGuardrail(75.0)
    product1.setCompetitors(List[Competitor](product1Competitor1, product1Competitor2))

    val product2Competitor1 = new Competitor
    product2Competitor1.setName("1")
    product2Competitor1.setPrice(5000)
    product2Competitor1.setWeight(20)

    val product2Competitor2 = new Competitor
    product2Competitor2.setName("2")
    product2Competitor2.setPrice(6000)
    product2Competitor2.setWeight(50)

    val product2Competitor3 = new Competitor
    product2Competitor3.setName("3")
    product2Competitor3.setPrice(6050)
    product2Competitor3.setWeight(30)

    val product2 = new ProductMetric
    product2.setProductID("id2")
    product2.setMatrixID("matrix1")
    product2.setLocationLevel2("level2")
    product2.setPsku("Product1")
    product2.setCategoryID("2")
    product2.setVolume(1000)
    product2.setLowerGuardrail(5000)
    product2.setUpperGuardrail(7000)
    product2.setCompetitors(List[Competitor](product2Competitor1, product2Competitor2, product2Competitor3))

    val product3Competitor1 = new Competitor
    product3Competitor1.setName("1")
    product3Competitor1.setPrice(3000)
    product3Competitor1.setWeight(10)

    val product3Competitor2 = new Competitor
    product3Competitor2.setName("2")
    product3Competitor2.setPrice(4000)
    product3Competitor2.setWeight(40)

    val product3Competitor3 = new Competitor
    product3Competitor3.setName("3")
    product3Competitor3.setPrice(5050)
    product3Competitor3.setWeight(50)

    val product3 = new ProductMetric
    product3.setProductID("id2")
    product3.setMatrixID("matrix1")
    product3.setLocationLevel2("level2")
    product3.setPsku("Product2")
    product3.setCategoryID("2")
    product3.setVolume(2000)
    product3.setLowerGuardrail(2000)
    product3.setUpperGuardrail(5500)
    product3.setCompetitors(List[Competitor](product2Competitor1, product2Competitor2, product2Competitor3))

    List[ProductMetric](product1, product2, product3)
  }
}

 case class PriceCalculator(productMetric: ProductMetric = new ProductMetric){

  def productPrices: ProductPrices = {
    val product: ProductPrices = new ProductPrices
    product.setProductID(productMetric.getProductID)
    product.setPsku(productMetric.getPsku())
    product.setMatrixID(productMetric.getMatrixID)
    product.setCategoryID(productMetric.getCategoryID)
    product.setLocationLevel2(productMetric.getLocationLevel2)
    product.setVolume(productMetric.getVolume)

    product
  }

  def forcePrice: Double = {
    val competitors = productMetric.competitors

    val numerator = competitors.foldLeft(0.0)((a,b) => a + (b.getPrice * b.getWeight))
    val denominator = competitors.foldLeft(0.0)(_ + _.getWeight)

    numerator/denominator
  }

  def guardRailPrice(forcePrice: Double): Double = {
    object withinGuardRailBoundary {
      def unapply(arg: Double): Boolean =
        (productMetric.getLowerGuardrail < arg) && (arg < productMetric.getUpperGuardrail)
    }

    object closestToLowerBound {
      def unapply(arg: Double): Boolean =
        Math.abs(productMetric.getLowerGuardrail - arg) < Math.abs(arg - productMetric.getUpperGuardrail)
    }

    object closestToUpperBound {
      def unapply(arg: Double): Boolean =
        Math.abs(arg - productMetric.getUpperGuardrail) < Math.abs(productMetric.getLowerGuardrail - arg)
    }

    forcePrice match {
      case withinGuardRailBoundary() => forcePrice
      case closestToLowerBound() => productMetric.getLowerGuardrail
      case closestToUpperBound() => productMetric.getUpperGuardrail
    }
  }

  def roundedPrice(guardRailPrice: Double): Double =
    Math.round(guardRailPrice/5) * 5

  def categoryPrice(preCalcProdPrices: ProductPrices, productsPrices: ListBuffer[ProductPrices]):Double = {
    val categoryProducts = productsPrices.groupBy(pp => pp.getCategoryID)
    val maxRoundedPrice = categoryProducts(preCalcProdPrices.getCategoryID).maxBy(_.getRoundedPrice).getRoundedPrice

    (maxRoundedPrice + preCalcProdPrices.getRoundedPrice) / 2
  }

   def matrixPercentage(preCalcProdPrices: ProductPrices, productsPrices: ListBuffer[ProductPrices]): Double = {
     val pmidGroupedProducts = productsPrices.groupBy(pp => (pp.getProductID, pp.getLocationLevel2))
     val matrixVolume = pmidGroupedProducts(preCalcProdPrices.getProductID, preCalcProdPrices.getLocationLevel2).foldLeft(0.0)(_ + _.getVolume)

     preCalcProdPrices.volume / matrixVolume
   }

   def aggregatedRoundedPrice(productsPrices: ListBuffer[ProductPrices]): Unit = {
    val groupedProducts = productsPrices.groupBy(pp => (pp.getPsku, pp.getLocationLevel2))
     groupedProducts.foreach(gp => {
       val roundedPriceSum = gp._2.foldLeft(0.0)(_ + _.getRoundedPrice)
       val totalRecords = gp._2.size

       val aggregatedRoundedPrice = roundedPriceSum / totalRecords
       printf("Aggregated total price for PSKU - %s and LocationLevel2 - %s is %s\n", gp._1._1, gp._1._2, aggregatedRoundedPrice)
     })
   }
}
