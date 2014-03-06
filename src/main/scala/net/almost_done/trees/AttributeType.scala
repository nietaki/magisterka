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
  def parseStringRepresentation(representation: String): Option[ValueType]

  /**
   * @param valueOption
   * @return the string representation of the value
   */
  def retrieveStringRepresentation(valueOption: Option[ValueType]): String = valueOption match {
    case Some(value) => retrieveStringRepresentation(value);
    case None => defaultNoneString
  }
  def retrieveStringRepresentation(value: ValueType): String;

  val defaultNoneString: String = "?"
}

object AttributeType {
  val unknownStrings: Set[String] = Set("?", "unknown")
}
