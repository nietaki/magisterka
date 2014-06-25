import org.apache.spark.SparkContext

/**
 * Created by nietaki on 3/6/14.
 */
object SimpleApp {
  /*
  def main(args: Array[String]) {
    val logFile = "README.md" // Should be some file on your system
    //spark_home - Location where Spark is installed on cluster nodes.
    val sc = new SparkContext("local[4]", "Simple App", "/home/nietaki/incubator-spark-typo",
    //val sc = new SparkContext("spark://xebab:7077", "Simple App", "/home/nietaki/incubator-spark",
        List("target/scala-2.10/magisterka_2.10-0.1.jar"))
    val logData = sc.textFile(logFile, 2).cache()
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println("Lines with a: %s, Lines with b: %s".format(numAs, numBs))
  }
  */
}
