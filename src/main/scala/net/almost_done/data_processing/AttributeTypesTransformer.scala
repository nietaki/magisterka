package net.almost_done.data_processing

/**
 * Created by nietaki on 3/3/14.
 */

import net.almost_done.data_processing.attributes._
import org.apache.spark.rdd._
class AttributeTypesTransformer(val lines: RDD[String]) extends java.io.Serializable{

  val nameCollections = lines.map({ line =>
    /* processing a single line - a single attribute */
    val strings = DataTransformer.sanitizeAndSplitRow(line)
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

  lazy val notIgnoredMask: Seq[Boolean] = attributeTypes.map( _ match {
    case Ignore(_) => false
    case _ => true
  })

  lazy val attributeTypesWithoutIgnore: Vector[AttributeType] = {
    attributeTypes.filterNot( _ match {
      case Ignore(_) => true
      case _ => false
    })
  }

  def removeIgnoredValues(attributeValues: Seq[String]): Seq[String] = {
    assert(attributeValues.length == notIgnoredMask.length)
    attributeValues.zip(notIgnoredMask).filter(_._2).map(_._1)
  }


}
