package net.almost_done.trees

/**
 * attribute of the type "Ignore" should not be taken into account when learning
 */
case object Ignore extends AttributeType {
  type ValueType = Any

  override def retrieveRepresentation(value: ValueType): String = "?"

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case _ => None
  }
}
