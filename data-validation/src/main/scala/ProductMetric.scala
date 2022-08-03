import scala.beans.BeanProperty

class ProductMetric {
  @BeanProperty
  var lowerGuardrail: Double = _
  @BeanProperty
  var upperGuardrail: Double = _
  @BeanProperty
  var volume: Int = _
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
  var competitors: List[Competitor] = _
}


class Competitor {
  @BeanProperty
  var name: String = _
  @BeanProperty
  var price: Double = _
  @BeanProperty
  var weight: Double = _
}
