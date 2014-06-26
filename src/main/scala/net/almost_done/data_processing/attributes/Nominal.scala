package net.almost_done.data_processing.attributes

import scala.collection.immutable.Vector

/**
 * Nominal has a definite number of distinct values
 */
case class Nominal(val name: String, values: Array[String]) extends AttributeType {
  type ValueType = Int
  val nominalValues: Vector[String] = Vector(values: _*)

  val nameIndices: Map[String,ValueType] = nominalValues.zipWithIndex.toMap

  override def retrieveRepresentation(value: ValueType): String = nominalValues(value)

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] = nameIndices.andThen(Some(_))

  override def attributeRepresentation: String = "Nominal: " + nominalValues.reduce(_ + ", " + _)

  override def parseRepresentationToValue(representation: String): AttributeValue = AttributeValue(parseRepresentation(representation))
}
