package net.almost_done.trees

/**
 * attribute of the type "Ignore" should not be taken into account when learning
 */
case object Ignore extends AttributeType {
  type ValueType = Any

  override def retrieveStringRepresentation(value: ValueType): String = "?"

  override def parseStringRepresentation(representation: String): Option[ValueType] = None
}
