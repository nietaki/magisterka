package net.almost_done.data_processing

/**
 * Created by nietaki on 3/3/14.
 */

import net.almost_done.data_processing.attributes._
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

  def getAttributeType(name: String, attributeValues: Array[String]): AttributeType = {
    assert(attributeValues.length > 0)
    val firstAttribute = attributeValues.head

    firstAttribute match {
      case "ignore" => Ignore(name)
      case "continuous-integer" => ContinuousInteger(name)
      case "continuous-float" => ContinuousDouble(name)
      case "continuous-double" => ContinuousDouble(name)
      case _ => Nominal(name, attributeValues)
    }
  }

  /*
   the actual output attributes collections
  */
  lazy val attributeTypes: Vector[AttributeType] = attributeNames.zip(attributeValuesCollection).map({case (n, v) => getAttributeType(n, v)})

  lazy val attributeTypesWithoutIgnore: Vector[AttributeType] = {
    attributeTypes.filterNot( _ match {
      case Ignore(_) => true
      case _ => false
    })
  }


}
