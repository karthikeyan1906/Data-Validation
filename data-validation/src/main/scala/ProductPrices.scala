import scala.beans.BeanProperty

class ProductPrices {
  @BeanProperty
  var forcePrice: Double = _
  @BeanProperty
  var guardRailPrice: Double = _
  @BeanProperty
  var roundedPrice: Double = _
  @BeanProperty
  var categoryPrice: Double = _
  @BeanProperty
  var matrixPercentage: Double = _
  @BeanProperty
  var aggRoundedPrice: Double = _
  @BeanProperty
  var categoryID: String = _
  @BeanProperty
  var psku: String = _
  @BeanProperty
  var locationLevel2: String = _
  @BeanProperty
  var matrixID: String = _
  @BeanProperty
  var productID: String = _
  @BeanProperty
  var volume: Int = _
}
