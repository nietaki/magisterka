package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */
class ContinuousDouble(val name: String, val columnIndex: Int) extends AttributeType[Double]  {
  type ValueType = Double
  override def attributeRepresentation: String = "Continuous Double"

  override def retrieveRepresentation(value: Double): String = value.toString

  //TODO catch the exception here and do another partial function that will drop the None, same in Integer
  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case repr => {
      Some(repr.toDouble) //yes, throwing here
    }
  }
}
