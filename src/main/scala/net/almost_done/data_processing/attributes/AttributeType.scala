package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */

/**
 * a base class for different attribute types - numeric and nominal, with methods for parsing them both ways
 *
 * also contains the name of the attribute
 */
abstract class AttributeType {
  type ValueType

  val name: String

  /**
   * @return an easily understandable definition of the attribute
   */
  def attributeRepresentation: String

  override def toString() = s"$name: ${this.attributeRepresentation}"
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
