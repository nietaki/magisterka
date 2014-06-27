package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */
case class ContinuousInteger(val name: String, val columnIndex: Int) extends AttributeType[Int] {
  type ValueType = Int

  override def attributeRepresentation: String = "Continous Integer"

  override def retrieveRepresentation(value: ValueType): String = value.toString

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case repr => {
      Some(repr.toInt) //yes, throwing here
    }
  }

}
