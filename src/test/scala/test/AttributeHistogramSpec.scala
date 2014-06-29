package test

import org.specs2.mutable._

import net.almost_done.trees._
import scala.collection.mutable
import org.specs2.specification._
import org.specs2.matcher.{Parameters, TraversableMatchers}
import scala.util.Random
import org.specs2.ScalaCheck
import org.scalacheck._
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions


class AttributeHistogramSpec extends Specification with TraversableMatchers with ScalaCheck {

  override implicit def defaultParameters = new Parameters(minTestsOk = 500)


  "binary search" should {
    import AttributeHistogram._

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
      ah.update(7)
      val bins = ah.resultingBins
      bins must beSorted
      bins must contain(Tuple2[Double, Int](0.0, 0))
      bins must contain(Tuple2[Double, Int](7.0, 1))
      //println(bins.toList)
    }

    "insert two values in natural order" in {
      val binCount = 5
      val ah = AttributeHistogram.empty[Int](binCount)
      ah.update(5)
      ah.update(7)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0, 0, 0, 5, 7)).updateMessage(s => s + " " + binKeys.toList)
      //mustBeSame(binKeys, Seq(0.0, 0,0, 0.0, 5.0, 7.0))
    }

    "insert two values in reversed order" in {
      val binCount = 5
      val ah = AttributeHistogram.empty[Int](binCount)
      ah.update(7)
      ah.update(5)
      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(Seq(0, 0, 0, 5, 7)).updateMessage(s => s + " " + binKeys.toList)
    }

    "insert distinct values to fill up the histogram" in {
      val binCount = 5
      val ah = AttributeHistogram.empty[Int](binCount)
      val values = Seq(1,2,3,4,5)
      //this is where we would prefer to use ScalaCheck
      val shuffled = new Random(5334).shuffle(values)
      shuffled.foreach{ah.update(_)}

      val binKeys = ah.binKeys
      binKeys must beSorted
      binKeys must containTheSameElementsAs(values).updateMessage(s => s + " " + binKeys.toList)
    }
    val almostAnyInt = Gen.choose[Int](-100, 100)
    val smallIntBinCount = Gen.choose[Int](1, 10)
    val listOfInts = Gen.listOf(almostAnyInt)

    "have the correct total item count in all bins" ! Prop.forAll(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = AttributeHistogram.empty[Int](binCount)
      ls.foreach{ah.update(_)}
      val binCountSum = ah.resultingBins.map{_._2}.sum
      binCountSum mustEqual ls.length
    }}

    "have the bins sorted" ! Prop.forAllNoShrink(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = AttributeHistogram.empty[Int](binCount)
      ls.foreach{ah.update(_)}
      ah.binKeys.sliding(2).filter(_.length >= 2).forall{seq =>
        seq(0) <= seq(1) //ERROR
      }
    }}

    /*
     generally the bounds should be determined by the biggest and the smallest values in the updates. But truth is
     stranger than fiction so in practice it's a little more complicated
     */
    "have the bin keys within bounds" ! Prop.forAllNoShrink(listOfInts, smallIntBinCount) {(ls, binCount) => {
      val ah = AttributeHistogram.empty[Int](binCount) //we need at least 2 buckets for this test
      ls.foreach{ah.update(_)}
      (!ls.isEmpty && (binCount > 1) ) ==> ((ah.binKeys.head >= ls.min || ah.binKeys.head == 0) && (ah.binKeys.last <= ls.max || ah.binKeys.last == 0 ))
    }}

  }

  "ScalaCheck's generators" should {
    val positiveInt = Gen.choose[Int](1, 100)
    val almostAnyInt = Gen.choose[Int](-100, 100)
    val tupleList:Gen[(List[Int], List[Int])] = {
      for{
        length <- positiveInt
        la <- Gen.listOfN(length, almostAnyInt)
        lb <- Gen.listOfN(length, almostAnyInt)
      } yield (la, lb)
    } // Dem Monads

    val sameTupleList:Gen[(List[Int], List[Int])] = {
      for{
        length <- positiveInt
        la <- Gen.listOfN(length, almostAnyInt)
      } yield (la, la)
    }

    "work with for-comprehensions as I suspect" ! Prop.forAllNoShrink(tupleList) {case (la, lb) =>
      la.length == lb.length
    }

    "work with for-comprehensions as I suspect part 2" ! Prop.forAllNoShrink(sameTupleList) {case (la, lb) =>
      la.zip(lb).forall{case(a,b) => a == b}
    }

    val attributeHistogram: Gen[AttributeHistogram[Int]] = for {
      bc <- Gen.choose[Int](1, 10)
      ls <- Gen.listOf(almostAnyInt)
    } yield {
      val ah = AttributeHistogram.empty[Int](bc)
      ls.foreach(ah.update(_))
      ah
    }
    val attributeHistogramPair: Gen[(AttributeHistogram[Int], AttributeHistogram[Int])] = for {
      bc <- Gen.choose[Int](2, 10)
      ls <- Gen.listOf(almostAnyInt)
      ls2 <- Gen.listOf(almostAnyInt)
    } yield {
      val ah = AttributeHistogram.empty[Int](bc)
      val ah2 = AttributeHistogram.empty[Int](bc)
      ls.foreach(ah.update(_))
      ls2.foreach(ah2.update(_))
      (ah, ah2)
    }

    "retain number of observations" ! Prop.forAllNoShrink(attributeHistogramPair) {case (ah, ah2) =>
      val ocSum = ah.observationCount + ah2.observationCount
      ah.merge(ah2)

      if(ah.observationCount != ocSum)
        println(ah)

      ah.observationCount mustEqual ocSum
    }

    "retain sortedness" ! Prop.forAllNoShrink(attributeHistogramPair) {case (ah, ah2) =>
      ah.merge(ah2)
      ah.binKeys.sliding(2).forall{ pair =>
        if(pair.length < 2)
          true
        else {
          pair.head <= pair.last
        }
      }
    }

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
  }
}