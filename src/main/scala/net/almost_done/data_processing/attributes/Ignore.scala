package net.almost_done.data_processing.attributes

/**
 * attribute of the type "Ignore" should not be taken into account when learning
 */
case class Ignore(val name: String) extends AttributeType {
  type ValueType = Int //for AttributeType to work

  override def attributeRepresentation: String = "Ignored"

  override def retrieveRepresentation(value: ValueType): String = "?"

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case _ => None
  }

  override def parseRepresentationToValue(representation: String): AttributeValue = AttributeValue[ValueType](None)
}
