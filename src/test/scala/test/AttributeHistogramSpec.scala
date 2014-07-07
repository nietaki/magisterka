package test

import net.almost_done.trees._
import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Parameters, TraversableMatchers}
import org.specs2.mutable._
import spire.implicits._
import spire.math._

import scala.util.Random // provides infix operators, instances and conversions
import Generators._

class AttributeHistogramSpec extends Specification with TraversableMatchers with ScalaCheck {

  override implicit def defaultParameters = new Parameters(minTestsOk = 500)


  "AttributeHistogram vector insert in order helper" should {
    "return vector of the right length" ! prop{ (v: Vector[Int], i: Int) =>
      AttributeHistogram.withInsertedInOrder(v.sorted, i).length == v.length + 1
    }

    "return a sorted vector" ! prop{(v: Vector[Int], i: Int) =>
      val withInserted = AttributeHistogram.withInsertedInOrder(v.sorted, i)
      withInserted.sliding(2).forall(v => v.head <= v.last)
    }

    "contain the inserted value" ! prop{(v: Vector[Int], i: Int) =>
      val withInserted = AttributeHistogram.withInsertedInOrder(v.sorted, i)
      withInserted.contains(i)
    }

  }


  "any AttributeHistogram" should {
    "have its keys sorted with Doubles" ! Prop.forAllNoShrink(Generators.doubleAH) {ah =>
      MatcherHelpers.isSorted(ah.binKeys)
    }

    "have its keys sorted with Ints" ! Prop.forAllNoShrink(Generators.intAH) {ah =>
      MatcherHelpers.isSorted(ah.binKeys)
    }

    "have its keys sorted after a series of updates" ! Prop.forAll(Generators.smallNumericList[Double]){ doubles: List[Double] =>
      val ah = AttributeHistogram.empty[Double](7)
      val newAh = doubles.foldLeft(ah)(_.updated(_))
      MatcherHelpers.isSorted(newAh.binKeys)
    }

    "keep the count of inserted values" ! prop {intList: List[Int] =>
      val ah = AttributeHistogram.empty[Int](7)
      val newAh = intList.foldLeft(ah)(_.updated(_))
      newAh.observationCount == intList.length
    }

    "have its min and max buckets within the bounds of inserted values" ! prop{intList: List[Int] =>
      if(intList.isEmpty) {
        true
      }else{
        val ah = AttributeHistogram.empty[Int](7)
        intList.foreach(ah.updated(_))
        val minKey = min(intList.min, 0)
        val maxKey = max(intList.max, 0)
        val binKeys = ah.binKeys
        binKeys.head >= minKey && binKeys.last <= maxKey
      }
    }
  }

  "Attribute Histogram binary search" should {
    import net.almost_done.trees.AttributeHistogram._

    /* http://etorreborre.github.io/specs2/guide/org.specs2.guide.Matchers.html#ScalaCheck */
    "be able to include scalacheck tests" ! prop { (a: Int) => a + a == 2 * a }

    val almostAnyInt = Gen.choose[Int](-10000, 10000)
    val listOfInts = Gen.listOf(almostAnyInt)
    val sortedArrayOfInts = listOfInts.map({ ls =>
      val arr = ls.toArray
      Sorting.mergeSort[Int](arr)
      arr
    })

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

    "find a position before a value greater or equal than X - land at the end of a sequence" ! Prop.forAll(sortedArrayOfInts, almostAnyInt) {(arr, x) =>{
      val pos = binarySearch(arr, x)
      if(pos < arr.length - 1) {
        arr(pos + 1) > x
      } else true
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
      val ah = AttributeHistogram.empty[Int](binCount)
      val ah2 = ah.updated(7)
      val bins = ah2.bins
      bins must beSorted
      bins must contain(Tuple2[Double, Int](0.0, 0))
      bins must contain(Tuple2[Double, Int](7.0, 1))
      //println(bins.toList)
    }

    "insert two values in natural order correctly" in {
      val binCount = 5
      var ah = AttributeHistogram.empty[Int](binCount)
      ah = ah.updated(5).updated(7)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0, 0, 0, 5, 7)).updateMessage(s => s + " " + binKeys.toList)
      //mustBeSame(binKeys, Seq(0.0, 0,0, 0.0, 5.0, 7.0))
    }

    "insert any two values without breaking sorting" in prop{(i1: Int, i2: Int) =>
      val binCount = 5
      var ah = AttributeHistogram.empty[Int](binCount)
      ah = ah.updated(i1).updated(i2)
      MatcherHelpers.isSorted(ah.binKeys)
    }

    "when inserting two values, they should be in the bin keys" in  Prop.forAll(smallInt, smallInt) { (i1: Int, i2: Int) =>
      val binCount = 5
      var ah = AttributeHistogram.empty[Int](binCount)
      ah = ah.updated(i1).updated(i2)
      if(! ah.binKeys.contains(i1) && ah.binKeys.contains(i2)) {
        println(ah.binKeys)
        false
      } else {
        true
      }
    }

    "insert two values in reversed order correctly" in {
      val binCount = 5
      var ah = AttributeHistogram.empty[Int](binCount)
      ah = ah.updated(7).updated(5)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0, 0, 0, 5, 7)).updateMessage(s => s + " " + binKeys.toList)
    }

    "insert distinct values to fill up the histogram" in {
      val binCount = 5
      val values = Seq(1,2,3,4,5)
      //this is where we would prefer to use ScalaCheck
      val shuffled = new Random(5334).shuffle(values)
      val ah = shuffled.foldLeft(AttributeHistogram.empty[Int](binCount))(_.updated(_))

      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(values).updateMessage(s => s + " " + binKeys.toList)
    }

    val almostAnyInt = Gen.choose[Int](-100, 100)
    val smallIntBinCount = Gen.choose[Int](1, 10)
    val listOfInts = Gen.listOf(almostAnyInt)

    "have the correct total item count in all bins" ! Prop.forAll(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = ls.foldLeft(AttributeHistogram.empty[Int](binCount))(_.updated(_))

      val binCountSum = ah.bins.map{_._2}.sum
      binCountSum mustEqual ls.length
    }}

    "have the bins sorted" ! Prop.forAllNoShrink(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = ls.foldLeft(AttributeHistogram.empty[Int](binCount))(_.updated(_))
      ah.binKeys.sliding(2).filter(_.length >= 2).forall{seq =>
        seq(0) <= seq(1)
      }
    }}

    /*
     generally the bounds should be determined by the biggest and the smallest values in the updates. But truth is
     stranger than fiction so in practice it's a little more complicated
     */
    "have the bin keys within bounds" ! Prop.forAllNoShrink(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = ls.foldLeft(AttributeHistogram.empty[Int](binCount))(_.updated(_))
      (!ls.isEmpty && (binCount > 1) ) ==> ((ah.binKeys.head >= ls.min || ah.binKeys.head == 0) && (ah.binKeys.last <= ls.max || ah.binKeys.last == 0 ))
    }}

  }

  "ScalaCheck's generators" should {
    import test.Generators._

    "work with for-comprehensions as I suspect" ! Prop.forAllNoShrink(smallIntTupleList) { case (la, lb) =>
      la.length == lb.length
    }

    "work with for-comprehensions as I suspect part 2" ! Prop.forAllNoShrink(sameTupleList) { case (la, lb) =>
      la.zip(lb).forall { case (a, b) => a == b}
    }
  }

  "two histograms merge" should {
    import test.Generators._

    "retain number of observations" ! pending
    Prop.forAllNoShrink(intAH, intAH) {(ah, ah2) =>
      val ocSum = ah.observationCount + ah2.observationCount
      val ah3 = ah.merged(ah2)

      ah3.observationCount mustEqual ocSum
    }

    "retain sortedness" ! pending
    Prop.forAllNoShrink(intAH, intAH) {case (ah, ah2) =>
      val ah3 = ah.merged(ah2)
      MatcherHelpers.isSorted(ah3.binKeys)
    }
    /*
    "not decrease the minimum bin key difference" ! Prop.forAllNoShrink(intAH, intAH) { (ah, ah2) =>
      def minKeyDiff[T: Numeric: Ordering](ah: AttributeHistogram[T]) = {
        ah.binKeys.sliding(2).map(ls => ls.last - ls.head).min
      }
      val actualMinKeyDiffBefore = min(minKeyDiff(ah), minKeyDiff(ah2))
      actualMinKeyDiffBefore <= minKeyDiff(ah.merge(ah2))
    }
    */


    /*
    "retain weighted average" ! Prop.forAllNoShrink(attributeHistogramPair) {case (ah, ah2) =>
      if(ah.observationCount == 0 && ah2.observationCount == 0) {
        0.0 must be closeTo(0.0, 0.1) //it's a hack!
      } else {
        val cummulativeWeightedAverage = (ah.weightedAverage * ah.observationCount + ah2.weightedAverage * ah2.observationCount) / (ah.observationCount + ah2.observationCount).toDouble
        ah.merge(ah2)

        //ah.weightedAverage must be closeTo(cummulativeWeightedAverage, math.abs(cummulativeWeightedAverage / 1000) + 0.01)
        ah.weightedAverage must be closeTo(cummulativeWeightedAverage, 1.0)
      }
    }
    */
  }

  "AttributeHistogram.shrinkBinVectorByOne" should {
    "decrease the size of the vector by one" ! Prop.forAll(intAH) { ha =>
      val shrunk =  AttributeHistogram.shrinkBinVectorByOne(ha.bins)
      shrunk.length == ha.bins.length - 1
    }
  }
}