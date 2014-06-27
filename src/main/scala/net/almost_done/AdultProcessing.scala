package net.almost_done

import java.io.File

import net.almost_done.data_processing.{DataTransformer, AttributeTypeFactory}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import shapeless._
import poly._

import scala.io.Source
import net.almost_done.data_processing.{AttributeTypeFactory => ATF}

/**
 * Created by nietaki on 3/6/14.
 */
object AdultProcessing {
  def main(args: Array[String]) {
    val conf = new SparkConf()
      .setMaster("local[4]")
      .setAppName("AdultProcessing")
      .set("spark.executor.memory", "3g")
      .set("spark.default.parallelism", "16")
    //spark_home - Location where Spark is installed on cluster nodes.
    /*
    val sc = new SparkContext("local[2]", "Simple App", "/home/nietaki/incubator-spark-typo",
        //val sc = new SparkContext("spark://xebab:7077", "Simple App", "/home/nietaki/incubator-spark",
        List("target/scala-2.10/magisterka_2.10-0.1.jar"))
    */
    val sc = new SparkContext(conf)
    val dataPath = "data/census/processed/census-income.data"

    val namePath = "data/census/processed/census-income.names" // Should be some file on your system
    val nameFile = new File(namePath)
    val lines: collection.Iterator[String] = Source.fromFile(nameFile).getLines()
    val linesWithIndices = lines.zipWithIndex
    val arguments: collection.Iterator[(String, Int, Array[String])] = linesWithIndices.map({case (line, index) =>
      val fields = DataTransformer.sanitizeAndSplitRow(line)
      val name = fields.head
      val values = fields.tail
      (name, index, values)
    })

    //val AttributeTypes = CensusData.attributeTypes
    val AttributeTypes = CensusData.attributeTypesWithDecision

    val censusData: RDD[String] = sc.textFile(dataPath, 12)
    val censusDataPreprocessed = censusData.map { row =>
      val values = DataTransformer.sanitizeAndSplitRow(row)
      /*
      //att.removeIgnoredValues(values)
      */
      //row + " bar"
    }

    val res = censusDataPreprocessed.collect()
    /*res.foreach{ arr =>
      arr.foreach(print(_))
      println()
      println()
    }*/
    println(res.length)
    //println(censusDataPreprocessed.count())
    //res.foreach(println(_))
  }
}
