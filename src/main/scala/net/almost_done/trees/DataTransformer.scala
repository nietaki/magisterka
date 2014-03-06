package net.almost_done.trees

/**
 * Created by nietaki on 3/3/14.
 */

import org.apache.spark.rdd._
class DataTransformer(val lines: RDD[String]) {

  val nameCollections = lines.map({ line =>
    /* processing a single line - a single attribute */
    val noDot = line.stripSuffix(".") //remove the dots from the end
    val separators: Array[Char] = ":,".toCharArray //split into category and values
    val strings = noDot.split(separators).map{_.trim().replace(' ', '_')} //trim and change spaces to underscores
    assert(strings.length >= 2, "we need at least a category and one value type")
    strings
  }).collect()

  val attributeCount = nameCollections.length

  val attributeNames: Vector[String] = Vector(nameCollections.map{_.head} : _*)
  val attributeValuesCollection: Array[Array[String]] = nameCollections.map{_.tail}

  def getAttributeType(attributeValues: Array[String]): AttributeType = {
    assert(attributeValues.length > 0)
    val firstAttribute = attributeValues.head

    firstAttribute match {
      case "ignore" => Ignore
      case "continuous-integer" => ContinuousInteger
      case "continuous-float" => ContinuousDouble
      case "continuous-double" => ContinuousDouble
      case _ => Nominal(attributeValues)
    }
  }

  lazy val attributeTypes: Array[AttributeType] = attributeValuesCollection.map(getAttributeType(_))
  lazy val attributeTypesWithoutIgnore: Array[AttributeType] = {
    attributeTypes.filterNot( _ match {
      case Ignore => true
      case _ => false
    })
  }


}
