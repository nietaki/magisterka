package net.almost_done.data_processing.attributes

/**
 * attribute of the type "Ignore" should not be taken into account when learning
 */
case class Ignore(val name: String) extends AttributeType {
  type ValueType = Any

  override def attributeRepresentation: String = "Ignored"

  override def retrieveRepresentation(value: ValueType): String = "?"

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case _ => None
  }

  /**
   * @return an easily understandable definition of the attribute
   */
}
