package test

import org.specs2.mutable._

import net.almost_done.trees._
import scala.collection.mutable
import org.specs2.specification._
import org.specs2.matcher.TraversableMatchers
import scala.util.Random


class AttributeHistogramSpec extends Specification with TraversableMatchers {
 
  "binary search" should {
    import AttributeHistogram._
    "find any element" in {
      val arr = Array(10, 11, 12, 13, 14)
      binarySearch(arr, 10) mustEqual 0
      binarySearch(arr, 11) mustEqual 1
      binarySearch(arr, 12) mustEqual 2
      binarySearch(arr, 13) mustEqual 3
      binarySearch(arr, 14) mustEqual 4
    }

    "if not found, choose bigger of the two (even outside of range)" in {
      val arr = Array(10, 12, 14, 16)
      binarySearch(arr, 9) mustEqual 0
      binarySearch(arr, 11) mustEqual 1
      binarySearch(arr, 13) mustEqual 2
      binarySearch(arr, 15) mustEqual 3
      binarySearch(arr, 1000) mustEqual 4
    }

    "behave correctly if all values are the same" in {
      binarySearch(Array(5,5,5,5), 6) mustEqual 4
      binarySearch(Array(5,5,5,5), 4) mustEqual 0
    }
  }

  "AttributeHistogram.update" should {
    def mustBeSame[T](as: Seq[T], bs: Seq[T]) = {
      as.length mustEqual bs.length
      as.zip(bs).foreach{case (a, b) => a mustEqual b}
    }
    "addValue" in {
      val binCount = 5
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      ah.update(7)
      val bins = ah.resultingBins
      bins must beSorted
      bins must contain(Tuple2[Double, Int](0.0, 0))
      bins must contain(Tuple2[Double, Int](7.0, 1))
      //println(bins.toList)
    }

    "insert two values in natural order" in {
      val binCount = 5
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      ah.update(5)
      ah.update(7)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0.0, 0.0, 0.0, 5.0, 7.0)).updateMessage(s => s + " " + binKeys.toList)
      //mustBeSame(binKeys, Seq(0.0, 0,0, 0.0, 5.0, 7.0))
    }

    "insert two values in reversed order" in {
      val binCount = 5
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      ah.update(7)
      ah.update(5)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0.0, 0.0, 0.0, 5.0, 7.0)).updateMessage(s => s + " " + binKeys.toList)
    }

    "insert distinct values to fill up the histogram" in {
      val binCount = 5
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      val values = Seq(1,2,3,4,5)
      //this is where we would prefer to use ScalaCheck
      val shuffled = new Random(5334).shuffle(values)
      shuffled.foreach{ah.update(_)}

      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(values.map{_.toDouble}).updateMessage(s => s + " " + binKeys.toList)
    }
  }

}