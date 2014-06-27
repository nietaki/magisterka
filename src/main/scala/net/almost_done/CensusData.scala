package net.almost_done

import java.io.File
import net.almost_done.data_processing.attributes.Ignore

import scala.io.Source
import shapeless._
import poly._

import net.almost_done.data_processing.{AttributeTypeFactory => ATF, DataTransformer}
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

  val attributeTypesWithDecision = ATF.int(arguments.next()) ::  //age
    ATF.int(arguments.next()) :: //cow
    ATF.int(arguments.next()) :: //dir
    ATF.int(arguments.next()) :: //dor
    ATF.int(arguments.next()) :: //education
    ATF.int(arguments.next()) :: //academic
    ATF.int(arguments.next()) :: //wage
    ATF.int(arguments.next()) :: //enroll
    ATF.int(arguments.next()) :: //marital
    ATF.int(arguments.next()) :: //major industry code
    ATF.int(arguments.next()) :: //major occupation code
    ATF.int(arguments.next()) :: //race
    ATF.int(arguments.next()) :: //hispanic origin
    ATF.int(arguments.next()) :: //sex
    ATF.int(arguments.next()) :: //labor union
    ATF.int(arguments.next()) :: //reason of unemployment
    ATF.int(arguments.next()) :: //full or part...
    ATF.int(arguments.next()) :: //capital gains
    ATF.int(arguments.next()) :: //capital losses
    ATF.int(arguments.next()) :: //dividends
    ATF.int(arguments.next()) :: //tax filer
    ATF.int(arguments.next()) :: //region
    ATF.int(arguments.next()) :: //state
    ATF.int(arguments.next()) :: //detailed
    ATF.ignore(arguments.next()) :: //instance weight
    ATF.int(arguments.next()) :: //migration1
    ATF.int(arguments.next()) :: //migration2
    ATF.int(arguments.next()) :: //migration3
    ATF.int(arguments.next()) :: //live in this house
    ATF.int(arguments.next()) :: //migration4
    ATF.int(arguments.next()) :: //num-persons
    ATF.int(arguments.next()) :: //family members
    ATF.int(arguments.next()) :: //country of birth father
    ATF.int(arguments.next()) :: //country of birth self
    ATF.int(arguments.next()) :: //citizenship
    ATF.int(arguments.next()) :: //own business
    ATF.int(arguments.next()) :: //fill inc questionnaire
    ATF.int(arguments.next()) :: //veterans
    ATF.int(arguments.next()) :: //veterans2
    ATF.int(arguments.next()) :: //weeks
    ATF.int(arguments.next()) :: //year
    ATF.int(arguments.next()) :: HNil//decision

  val attributeTypes = attributeTypesWithDecision.init
  val decisionAttribute = attributeTypesWithDecision.last
  //val relevantAttributeTypes = attributeTypes.filterNot[Ignore]
}
