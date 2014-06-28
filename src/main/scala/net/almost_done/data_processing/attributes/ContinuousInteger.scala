package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */
class ContinuousInteger(val name: String, val columnIndex: Int) extends AttributeType[Int] {
  type ValueType = Int

  override def attributeRepresentation: String = "Continuous Integer"

  override def retrieveRepresentation(value: Int): String = value.toString

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case repr => {
      Some(repr.toInt) //yes, throwing here
    }
  }

}
