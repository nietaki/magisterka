package net.almost_done

import net.almost_done.data_processing.DataTransformer
import org.apache.spark.SparkContext

/**
 * Created by nietaki on 3/6/14.
 */
object AdultProcessing {
  def main(args: Array[String]) {

    //spark_home - Location where Spark is installed on cluster nodes.
    val sc = new SparkContext("local[4]", "Simple App", "/home/nietaki/incubator-spark-typo",
        //val sc = new SparkContext("spark://xebab:7077", "Simple App", "/home/nietaki/incubator-spark",
        List("target/scala-2.10/magisterka_2.10-0.1.jar"))

    val nameFile = "data/census/processed/census-income.names" // Should be some file on your system
    val dataFile = "data/census/processed/census-income.data"

    val nameData = sc.textFile(nameFile, 2).cache()
    val dt = new DataTransformer(nameData)

    dt.attributeTypes.foreach(println(_))
  }
}
