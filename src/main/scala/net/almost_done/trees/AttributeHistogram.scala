package net.almost_done.trees

/**
 * Created by nietaki on 3/7/14.
 */

import Ordering.Implicits._
import Numeric.Implicits._
import scala.annotation.tailrec
import scala.util.Sorting

/**
 * a *mutable* AttributeHistogram class this will make it much more performance-friendly
 * @param arr the mutable Array size of which determines the binCount
 * @param n the numeric extension class for T
 * @tparam T numeric type of the attribute, most commonly an Int or a Double
 */
class AttributeHistogram[T](val arr: Array[AttributeHistogram.Bin])(implicit n: Numeric[T]){
  import AttributeHistogram.Bin

  //require(arr.sliding(2).forall(vec => { //FIXME skip the last pair
  //  vec.head._1 <= vec.last._1
  //}))


  /**
   * bin count determined by the array size - one spot leftover for the inserted values
   */
  lazy val binCount = arr.length - 1


  def observationCount: Int = resultingBins.map(_._2).sum
  /** for convenience index of the last modified value */
  lazy val additionalBinIndex = arr.length - 1

  val resultingBins = arr.view(0, additionalBinIndex)

  def binKeys = resultingBins.map{pr => pr._1}.toSeq
  def binKeysAll = arr.map{pr => pr._1}.toSeq

  assert(resultingBins.length == arr.length - 1)


  /**
   * this could be done in a nicer fashion, but it would be less efficient.
   *
   * Low level array operations it is...
   * @param newOccurrence attribute value to add to the histogram
   */
  def update(newOccurrence: T): Unit = {
    /* PART 1 - insert */
    val updatingTuple = Tuple2(newOccurrence.toDouble(), 1)
    //arr.update(arr.length - 1, Tuple2(newOccurrence.toDouble(), 1))
    val withoutLast = arr.view(0, additionalBinIndex)

    /* this is where the new tuple should go - what value it should push towards the back */
    val targetIndex = AttributeHistogram.binarySearch(withoutLast, updatingTuple)

    //adding the new value and pushing the later ones forward
    var newTuple = updatingTuple;
    Range(targetIndex, additionalBinIndex).inclusive.foreach( idx => {
      val curTuple = arr(idx)
      arr.update(idx, newTuple)
      newTuple = curTuple
    })

    AttributeHistogram.shrinkBinArrayByOne(arr, additionalBinIndex)
  }


  def merge(other: AttributeHistogram[T]): Unit = {
    assert(this.resultingBins.length == other.resultingBins.length)
    val combinedBinsSorted = Sorting.stableSort(this.resultingBins ++ other.resultingBins)

    val workingArray = Array.fill[Bin](combinedBinsSorted.length + 1)((0.0, 0)) //TODO: this could be more efficient
    combinedBinsSorted.copyToArray(workingArray)
    Range(combinedBinsSorted.length, additionalBinIndex, -1).inclusive.foreach{idx => {
      AttributeHistogram.shrinkBinArrayByOne(workingArray, idx)
    }}

    workingArray.copyToArray(arr, 0, additionalBinIndex)
  }
  def sum = ???


  override def toString = resultingBins.toList.toString
}

object AttributeHistogram {
  import Numeric.Implicits._
  import Ordering.Implicits._

  type Bin = (Double, Int)

  def ZeroObject[T](binCount: Integer)(implicit n: Numeric[T]) = new AttributeHistogram[T](Array.fill(binCount + 1)(Tuple2(0.0, 0)))

  def binarySearch[A <% Ordered[A]](a: IndexedSeq[A], v: A) = {
    @tailrec
    def recurse(low: Int, high: Int): Int = (low + high) / 2 match {
      //case _ if high < low => None
      case _ if high < low => low //returning the bigger index of the two
      case mid if a(mid) > v => recurse(low, mid - 1)
      case mid if a(mid) < v => recurse(mid + 1, high)
      case mid => mid
    }
    recurse(0, a.size - 1)
  }

    /**
   * merges the appropriate bins together, effectively reducing binArray's size by 1
   * @param binArray the binArray the method should be modifying
   * @param currentItemCount currentItemCount. The Array should be bigger than currentItemCount by at least 1.
   */
  protected def shrinkBinArrayByOne(binArray: Array[Bin], currentItemCount: Int): Unit = {
    /* finding the tuples to combine */
    assert(binArray.length > currentItemCount)

    /* PART 2 - finding the tuples to combine */
    var minDifference = Double.PositiveInfinity
    var minDiffIdx: Int = 0;
    Range(0, currentItemCount).foreach(idx => {
      val diff = binArray(idx+1)._1 - binArray(idx)._1
      if(diff < minDifference) {
        minDiffIdx = idx
        minDifference = diff
      }

      if(binArray(idx)._2 == 0) {
        //the bin is empty, we can replace it no worries
        minDiffIdx = idx
        minDifference = 0.0
      }
    })

    val combined = combineBins(binArray(minDiffIdx), binArray(minDiffIdx + 1))

    /* PART 3 - assigning and combining */
    binArray.update(minDiffIdx, combined)
    Range(minDiffIdx + 1, currentItemCount).foreach(idx => {
      binArray.update(idx, binArray(idx + 1))
    })

  }

  protected def combineBins(b1: Bin, b2: Bin): Bin = {
    val (q1,k1) = b1
    val (q2,k2) = b2

    val k = k1 + k2
    if(k == 0) {
      //we don't want to divide by 0
      (0.0, 0)
    } else {
      val q = (q1 * k1 + q2 * k2) / (k1 + k2)
      (q, k)
    }
  }
}
