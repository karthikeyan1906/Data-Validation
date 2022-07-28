import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object DataValidation extends App {
  val sampleData = readSampleData()
  val headerRow = sampleData.head

 computeResult(sampleData.tail, headerRow)

  def readSampleData(): List[Array[String]] = {
    val src = Source.fromFile("src/assets/SampleData.csv")
    val iter = src.getLines().map(_.split(","))
    val sampleData = iter.toList
    src.close()

    sampleData
  }

  def computeResult(rowValues: List[Array[String]], rowHeaders: Array[String]): Unit= {
    @tailrec
    def validateRow(rows: List[Array[String]], header: Array[String]): Unit = {
      def validateField(values: Array[String]): Unit = {

        field1Validation(values(0), header(0))
        field2Validation(values(1), header(1))
        field3Validation(values(2), header(2), values(4))
        field4Validation(values(3), header(3))
        field5Validation(values(4), header(4))
      }

      if (rows.isEmpty) Unit
      else {
        validateField(rows.head)
        validateRow(rows.tail, header)
      }
    }

    validateRow(rowValues, rowHeaders)
  }

  def field1Validation(value: String, fieldName: String): Unit ={
    val results = ListBuffer[Option[String]]()
    val field1 = new DataValidation(value)
    results += field1.shouldBeText
    results += field1.shouldNotBeLongerThan20

    field1.printConsolidatedResult(results, fieldName)
  }

  def field2Validation(value: String, fieldName: String): Unit ={
    val results = ListBuffer[Option[String]]()
    val field2 = new DataValidation(value)
    results += field2.shouldBeText

    field2.printConsolidatedResult(results, fieldName)
  }

  def field3Validation(value: String, fieldName: String, toCompare: String): Unit ={
    val results = ListBuffer[Option[String]]()
    val field3 = new DataValidation(value)
    results += field3.shouldBeNumber
    results += field3.shouldBeLessThan(toCompare)

    field3.printConsolidatedResult(results, fieldName)
  }

  def field4Validation(value: String, fieldName: String): Unit ={
    val results = ListBuffer[Option[String]]()
    val field4 = new DataValidation(value)
    results += field4.shouldBeText
    results += field4.shouldBeDate

    field4.printConsolidatedResult(results, fieldName)
  }

  def field5Validation(value: String, fieldName: String): Unit ={
    val results = ListBuffer[Option[String]]()
    val field5 = new DataValidation(value)
    results += field5.shouldBeNumber

    field5.printConsolidatedResult(results, fieldName)
  }
}

class DataValidation(val value: String){
  def shouldBeText:Option[String] = {
    if (value.matches("^[a-zA-Z\\d-]+$".r.toString)) None
    else Some("should be text")
  }

  def shouldNotBeLongerThan20: Option[String] = {
    if (value.length() <= 20) None
    else Some("should not be longer than 20 chars")
  }

  def shouldBeNumber: Option[String] = {
    if (value.forall(_.isDigit)) None
    else Some("should be number")
  }

  def shouldBeDate: Option[String] = {
    if (value.matches("^\\d{2}-\\d{2}-\\d{4}$".r.toString)) None
    else Some("should be of format mm-dd-yyyy")
  }

  def shouldBeLessThan(field: String): Option[String] ={
    val lowerValue = parseDouble(value) getOrElse Double.MaxValue
    val higherValue = parseDouble(field) getOrElse Double.MinValue

    if (lowerValue < higherValue) None
    else Some("should be less than Field5")
  }

  def printConsolidatedResult(results: ListBuffer[Option[String]], fieldName: String): Unit = {
    val consolidatedResult = results.filter(r => r != None).flatten.mkString(" and ")
    if (consolidatedResult != "") println(fieldName + " " + consolidatedResult)
  }

  private def parseDouble(s: String): Option[Double] = try { Some(s.toDouble) } catch { case _ => None }
}

