package net.almost_done.data_processing

/**
 * Created by nietaki on 3/3/14.
 */

import net.almost_done.data_processing.attributes._

object AttributeTypeFactory {

  type Argument = (String, Int, Array[String], String)

  val doubleAttributeTupled: PartialFunction[Argument, AttributeType[Double]] = {
    case(name, columnIndex, attributeValues, "continuous-double") => new ContinuousDouble(name, columnIndex)
  }

  val intAttributeTupled: PartialFunction[Argument, AttributeType[Int]] = {
    case(name, columnIndex, attributeValues, "continuous-integer") => new ContinuousInteger(name, columnIndex)
      //TODO add ignored
    case(name, columnIndex, attributeValues, _) => new Nominal(name, columnIndex, attributeValues)
  }

  val attributeTupled: PartialFunction[Argument, AttributeType[_]] = {
    doubleAttributeTupled orElse intAttributeTupled
  }

  def attribute(name: String, columnIndex: Int, attributeValues: Array[String]) = {
    attributeTupled((name, columnIndex, attributeValues, attributeValues(0)))
  }

  def doubleAttribute(name: String, columnIndex: Int, attributeValues: Array[String]) = {
    doubleAttributeTupled((name, columnIndex, attributeValues, attributeValues(0)))
  }

  def intAttribute(name: String, columnIndex: Int, attributeValues: Array[String]) = {
    intAttributeTupled((name, columnIndex, attributeValues, attributeValues(0)))
  }

}
