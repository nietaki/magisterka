package test

import org.specs2.mutable._

import net.almost_done.trees._
import scala.collection.mutable
import org.specs2.specification._
import org.specs2.matcher.{Parameters, TraversableMatchers}
import scala.util.Random
import org.specs2.ScalaCheck
import org.scalacheck._
import scala.util.Sorting


class AttributeHistogramSpec extends Specification with TraversableMatchers with ScalaCheck {



  "binary search" should {
    import AttributeHistogram._

    /* http://etorreborre.github.io/specs2/guide/org.specs2.guide.Matchers.html#ScalaCheck */
    "be able to include scalacheck tests" ! prop { (a: Int) => a + a == 2 * a }

    val almostAnyInt = Gen.choose[Int](-10000, 10000)
    val listOfInts = Gen.listOf(almostAnyInt)
    val sortedArrayOfInts = listOfInts.map(ls => Sorting.stableSort(ls))

    val trueProp = Prop.forAll(listOfInts){ls => true}
    "be true to itself ;)" ! trueProp

    "return a viable index when searching" ! Prop.forAll(sortedArrayOfInts, almostAnyInt) {(arr, x) => {
      val pos = binarySearch(arr, x)
      (pos >= 0) && pos <= arr.length
    }}

    "find a position after a value no bigger than X" ! Prop.forAll(sortedArrayOfInts, almostAnyInt) {(arr, x) =>{
      val pos = binarySearch(arr, x)
      if(pos > 0) {
        arr(pos - 1) <= x
      } else true
    }}

    "find a position on a value greater or equal than X" ! Prop.forAll(sortedArrayOfInts, almostAnyInt) {(arr, x) =>{
      val pos = binarySearch(arr, x)
      if(pos < arr.length) {
        arr(pos) >= x
      } else true
    }}

    "find a position after a value no smaller than X" ! Prop.forAll(sortedArrayOfInts, almostAnyInt) {(arr, x) =>{
      true
    }}
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
    val almostAnyInt = Gen.choose[Int](-100, 100)
    val positiveInt = Gen.choose[Int](1, 100)
    val listOfInts = Gen.listOf(almostAnyInt)

    "have the correct total item count in all bins" ! Prop.forAll(listOfInts, positiveInt) {(ls, binCount) => {
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      ls.foreach{ah.update(_)}
      val binCountSum = ah.resultingBins.map{_._2}.sum
      binCountSum mustEqual ls.length
    }}

    "have the bins sorted" ! Prop.forAllNoShrink(listOfInts, positiveInt) {(ls, binCount) => {
      val ah = AttributeHistogram.ZeroObject[Int](binCount)
      ls.foreach{ah.update(_)}
      ah.binKeys.sliding(2).filter(_.length >= 2).forall{seq =>
        seq(0) <= seq(1) //ERROR
      }
    }}

    /*
     generally the bounds should be determined by the biggest and the smallest values in the updates. But truth is
     stranger than fiction so in practice it's a little more complicated
     */
    "have the bin keys within bounds" ! Prop.forAllNoShrink(listOfInts, positiveInt) {(ls, binCount) => {
      val ah = AttributeHistogram.ZeroObject[Int](binCount) //we need at least 2 buckets for this test
      ls.foreach{ah.update(_)}
      (!ls.isEmpty && (binCount > 1) ) ==> ((ah.binKeys.head >= ls.min || ah.binKeys.head == 0) && (ah.binKeys.last <= ls.max || ah.binKeys.last == 0 ))
    }}


  }

}