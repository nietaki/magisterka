package net.almost_done.data_processing

/**
 * Created by nietaki on 3/3/14.
 */

import net.almost_done.data_processing.attributes._

object AttributeTypeFactory {

  def doubleAttr(name: String, columnIndex: Int, attributeValues: Array[String]): AttributeType[Double] = {
    assert(attributeValues.length > 0)
    val firstAttribute = attributeValues.head

    firstAttribute match {
      case "continuous-double" => ContinuousDouble(name, columnIndex)
      case _ => throw new IllegalArgumentException()
    }
  }

  def double = (doubleAttr _).tupled

  def intAttr(name: String, columnIndex: Int, attributeValues: Array[String]): AttributeType[Int] = {
    assert(attributeValues.length > 0)
    val firstAttribute = attributeValues.head

    firstAttribute match {
      case "continuous-integer" => ContinuousInteger(name, columnIndex)
      case "continuous-double" => throw new IllegalArgumentException()
      case _ => Nominal(name, columnIndex, attributeValues)
    }
  }

  def int = (intAttr _).tupled

  def ignoreAttr(name: String, columnIndex: Int, attributeValues: Array[String]): Ignore = {
    assert(attributeValues.length > 0)
    Ignore(name, columnIndex)
  }
  def ignore = (ignoreAttr _).tupled

}
