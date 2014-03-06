package net.almost_done.trees

/**
 * Created by nietaki on 3/6/14.
 */
abstract class AttributeType {
  type ValueType

  /**
   * parses a string representation of the value to a an Option of a value of a given type
   * @param representation string representation of the type
   * @return the value option
   */
  def parseRepresentation(representation: String): Option[ValueType] = {
    (parseUnknown orElse parseRepresentationInner)(representation)
  }

  val parseUnknown: PartialFunction[String, Option[ValueType]] = {
    case str if AttributeType.unknownStrings.contains(str) => None
  }

  def parseRepresentationInner: PartialFunction[String, Option[ValueType]]

  /**
   * @param valueOption
   * @return the string representation of the value
   */
  def retrieveRepresentation(valueOption: Option[ValueType]): String = valueOption match {
    case Some(value) => retrieveRepresentation(value);
    case None => defaultNoneString
  }
  def retrieveRepresentation(value: ValueType): String;

  val defaultNoneString: String = "?"
}

object AttributeType {
  val unknownStrings: Set[String] = Set("?", "unknown")
}
