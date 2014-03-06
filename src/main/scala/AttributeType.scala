/**
 * Created by nietaki on 3/3/14.
 */

import scala.collection.immutable.Vector

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

/**
 * attribute of the type "Ignore" should not be taken into account when learning
 */
case object Ignore extends AttributeType {
  type ValueType = Null

  override def retrieveStringRepresentation(value: ValueType): String = "?"

  override def parseStringRepresentation(representation: String): Option[ValueType] = Some(null)
}

/**
 * Nominal has a definite number of distinct values
 */
case class Nominal(values: Array[String]) extends AttributeType {
  type ValueType = Int
  val nominalValues: Vector[String] = Vector(values: _*)

  val zipped: Map[String,ValueType] = nominalValues.zipWithIndex.toMap

  override def retrieveStringRepresentation(value: ValueType): String = nominalValues(value)

  override def parseStringRepresentation(representation: String): Option[ValueType] = zipped.get(representation)

  override def toString = "Nominal: " + nominalValues.reduce(_ + ", " + _)
}

case object ContinuousInteger extends AttributeType {
  type ValueType = Int

  override def retrieveStringRepresentation(value: ContinuousInteger.ValueType): String = value.toString

  override def parseStringRepresentation(representation: String): Option[ContinuousInteger.ValueType] =  {
    var ret: Option[ValueType] = None;
    try {
      Some(representation.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }
}
case object ContinuousDouble extends AttributeType  {
  type ValueType = Double

  override def retrieveStringRepresentation(value: ValueType): String = value.toString

  override def parseStringRepresentation(representation: String): Option[ValueType] =  {
    var ret: Option[ValueType] = None;
    try {
      Some(representation.toDouble)
    } catch {
      case _: NumberFormatException => None
    }
  }

}