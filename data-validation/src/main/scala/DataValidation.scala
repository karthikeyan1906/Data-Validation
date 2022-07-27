import scala.annotation.tailrec
import scala.io.Source

object DataValidation extends App {
  val sampleData = readSampleData()
  val constraints = sampleConstraints()
  val headerRow = sampleData.head

  val results =  computeResult(constraints, sampleData.tail, headerRow)

  if (results.isEmpty) println("Validation succeeded")
  else println("Validation failed for following fields:")

  results.foreach(println)


  def readSampleData(): List[Array[String]] = {
    val src = Source.fromFile("src/assets/SampleData.csv")
    val iter = src.getLines().map(_.split(","))
    val sampleData = iter.toList
    src.close()

    sampleData
  }

  def sampleConstraints(): Map[String, List[String]] = {
    Map(
      "Field1" -> List("text", "not longer than 20"),
      "Field2" -> List("text"),
      "Field3" -> List("number"),
      "Field4" -> List("text", "mm-dd-yyyy")
    )
  }

  def computeResult(constraints: Map[String, List[String]], rowValues: List[Array[String]], rowHeaders: Array[String]): List[String]= {
    @tailrec
    def validateRow(rows: List[Array[String]], headers: Array[String], aggregator: List[String]): List[String] = {
      @tailrec
      def validateField(values: Array[String], hd: Array[String], agg: List[String]): List[String] = {
        if (values.isEmpty) agg
        else validateField(values.tail, hd.tail, agg ++ fieldValidator(values.head, constraints(hd.head), hd.head))
      }

      if (rows.isEmpty) aggregator
      else validateRow(rows.tail, headers, aggregator ++ validateField(rows.head, headers, List[String]()))
    }

    validateRow(rowValues, rowHeaders, List[String]())
  }

  def fieldValidator(value: String, constraints:List[String], fieldName: String): List[String] = {
    constraints.map {
      case "text" =>
        if (value.matches("^[a-zA-Z\\d-]+$".r.toString)) ""
        else s"""$fieldName : "$value" is not a text"""
      case "not longer than 20" =>
        if (value.length() <= 20) ""
        else s"""$fieldName : "$value" should not be longer than 20 characters"""
      case "number" =>
        if (value.matches("^\\d+$".r.toString)) ""
        else s"""$fieldName : "$value" is not a number"""
      case "mm-dd-yyyy" =>
        if (value.matches("^\\d{2}-\\d{2}-\\d{4}$".r.toString)) ""
        else s"""$fieldName : "$value" should be in "mm-dd-yyyy" format"""
    }.filter(r => r != "")
  }
}

