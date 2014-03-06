package net.almost_done.trees

import scala.collection.immutable.Vector

/**
 * Nominal has a definite number of distinct values
 */
case class Nominal(values: Array[String]) extends AttributeType {
  type ValueType = Int
  val nominalValues: Vector[String] = Vector(values: _*)

  val zipped: Map[String,ValueType] = nominalValues.zipWithIndex.toMap

  override def retrieveStringRepresentation(value: ValueType): String = nominalValues(value)

  override def parseStringRepresentation(representation: String): Option[ValueType] = Some(zipped(representation)) //yes, we intend to throw here

  override def toString = "Nominal: " + nominalValues.reduce(_ + ", " + _)
}
