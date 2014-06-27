package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */

/**
 * a base class for different attribute types - numeric and nominal, with methods for parsing them both ways
 *
 * also contains the name of the attribute
 */
abstract class AttributeType[ValueType] {

  /* **fields and methods to be implemented in subclasses** */
  val name: String

  /**
   * the index of the attribute in a row
   */
  val columnIndex: Int

  /**
   * @return an easily understandable definition of the attribute, for debugging purpuses only
   */
  def attributeRepresentation: String

  /**
   *
   * @return the value option parsed from its String representation
   */
  protected def parseRepresentationInner: PartialFunction[String, Option[ValueType]]

  /**
   * a method for transforming the underlying values back to their string representations
   * @param value the value to be represented as string
   * @return the string representation of the value
   */
  def retrieveRepresentation(value: ValueType): String;


  /* **end of the methods to be implemented** */
  override def toString() = s"$name: ${this.attributeRepresentation}"

  /**
   * Given a row of attribute values, parses the corresponding one
   * @param row a sequence of string representations of attribute values
   * @return the corresponding attribute value
   */
  def parseFromRow(row: IndexedSeq[String]): Option[ValueType] = {
    parseRepresentation(row(columnIndex))
  }

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

  /**
   * @param valueOption
   * @return the string representation of the value
   */
  def retrieveRepresentation(valueOption: Option[ValueType]): String = valueOption match {
    case Some(value) => retrieveRepresentation(value);
    case None => defaultNoneString
  }


  val defaultNoneString: String = "?"
}

object AttributeType {
  val unknownStrings: Set[String] = Set("?", "unknown")
}
