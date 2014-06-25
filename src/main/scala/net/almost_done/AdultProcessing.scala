package net.almost_done

import net.almost_done.data_processing.{DataTransformer, AttributeTypesTransformer}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

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
    val nameFile = "data/census/processed/census-income.names" // Should be some file on your system
    val dataFile = "data/census/processed/census-income.data"

    val censusNames: RDD[String] = sc.textFile(nameFile, 4).cache()
    val att = new AttributeTypesTransformer(censusNames)

    att.attributeTypes.foreach(println(_))
    println(att.attributeTypes.length)

    val censusData: RDD[String] = sc.textFile(dataFile, 12)
    val censusDataPreprocessed = censusData.map { row =>
      //val values = DataTransformer.sanitizeAndSplitRow(row)
      val noDot = row.stripSuffix(".") //remove the dots from the end (if neccessary)
      val separators: Array[Char] = ":,".toCharArray //split category (if neccessary) and values
      noDot.split(separators).map{_.trim().replace(' ', '_')} //trim and change spaces to underscores
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
