package net.almost_done

import java.io.File
import net.almost_done.data_processing.attributes.Ignore

import scala.io.Source
import shapeless._
import poly._

import net.almost_done.data_processing.{AttributeTypeFactory, DataTransformer}
/**
 * Created by nietaki on 27.06.14.
 */
object CensusData {
  val namePath = "data/census/processed/census-income.names" // Should be some file on your system
  private val nameFile = new File(namePath)
  private val lines: collection.Iterator[String] = Source.fromFile(nameFile).getLines()
  private val linesWithIndices = lines.zipWithIndex
  private val arguments: collection.Iterator[(String, Int, Array[String])] = linesWithIndices.map({case (line, index) =>
    val fields = DataTransformer.sanitizeAndSplitRow(line)
    val name = fields.head
    val values = fields.tail
    (name, index, values)
  })

  val allAttributeTypesWithDecision = arguments.map(tuple => AttributeTypeFactory.attribute(tuple._1, tuple._2, tuple._3)).toArray
  allAttributeTypesWithDecision(24).ignored = true

  val decisionAttribute = allAttributeTypesWithDecision.last

  val allAttributeTypesWithoutDecision = allAttributeTypesWithDecision.init

  val attributeTypes = allAttributeTypesWithoutDecision.filterNot(_.ignored)
  

}
