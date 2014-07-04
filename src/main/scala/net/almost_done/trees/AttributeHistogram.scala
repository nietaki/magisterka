package net.almost_done.trees

/**
 * Created by nietaki on 3/7/14.
 */
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions
import scala.annotation.tailrec

/**
 * a *mutable* AttributeHistogram class this will make it much more performance-friendly
 * @param arr the mutable Array size of which determines the binCount
 * @tparam T numeric type of the attribute, most commonly an Int or a Double
 */
class AttributeHistogram[T: Numeric](val arr: Array[AttributeHistogram.Bin[T]]){
  import AttributeHistogram.Bin

  lazy val tZero = implicitly[Numeric[T]].zero

  /**
   * bin count determined by the array size - one spot leftover for the inserted values
   */
  lazy val additionalBinIndex = arr.length - 1

  def observationCount: Int = resultingBins.map(_._2).sum
  /** for convenience index of the last modified value */

  val resultingBins = arr.view(0, additionalBinIndex)

  def binKeys = resultingBins.map{pr => pr._1}.toSeq
  def binKeysAll = arr.map{pr => pr._1}.toSeq

  def weightedAverage: Double =
    if(observationCount == 0)
      0.0
    else
      resultingBins.map{case (k, v) => k.toDouble()*v}.sum / observationCount.toDouble

  /**
   * this could be done in a nicer fashion, but it would be less efficient.
   *
   * Low level array operations it is...
   * @param newOccurrence attribute value to add to the histogram
   */
  def update(newOccurrence: T): AttributeHistogram[T]= {
    /* PART 1 - insert */
    val updatingTuple = Tuple2(newOccurrence, 1)
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
    this
  }


  def merge(other: AttributeHistogram[T]): Unit = {
    assert(this.resultingBins.length == other.resultingBins.length)
    val combinedBinsSorted = (this.resultingBins ++ other.resultingBins).toArray
    Sorting.mergeSort(combinedBinsSorted)

    val workingArray = Array.fill[Bin[T]](combinedBinsSorted.length + 1)((tZero, 0)) //TODO: this could be more efficient
    combinedBinsSorted.copyToArray(workingArray)
    Range(combinedBinsSorted.length, additionalBinIndex, -1).inclusive.foreach{idx => {
      AttributeHistogram.shrinkBinArrayByOne(workingArray, idx)
    }}

    workingArray.copyToArray(arr, 0, additionalBinIndex)
  }

  /**
   *
   * @param b a point b such that p_1 < b < b_B
   * @return Estimated number of points in the interval [-\infty, b]
   */
  def sum(b: T): Double = {
    val i = AttributeHistogram.binarySearch(this.resultingBins, (b, 0))
    val bDouble = b.toDouble() //FIXME make sure we need to convert to double
    //TODO: replace arr with a lazy view with prepended and appended bins
    def doublify(b: Bin[T]): (Double, Double) ={
      (b._1.toDouble(), b._2.toDouble)
    }
    val (pi, mi) = doublify(arr(i))
    val (pi2, mi2) = doublify(arr(i+1))

    //TODO analyze the situation for when it's (either) the end of the array

    val mb: Double = mi+ (mi2 - mi)/ (pi2 - pi) * (bDouble - pi)
    val s: Double = (mi + mb) / 2 * (bDouble - pi)/(pi2 - pi)
    //the view does exclude the i-th bin
    val partialSum = arr.view(0, i).foldLeft(0){case (rollingSum, pr)  => rollingSum + pr._2}
    s + partialSum
  }

  override def toString = resultingBins.toList.toString
}

object AttributeHistogram {
  type Bin[T] = (T, Int)

  def empty[T: Numeric](binCount: Int) = new AttributeHistogram[T](Array.fill(binCount + 1)(Tuple2((implicitly[Numeric[T]].zero), 0)))

  def binarySearch[A: Order](a: IndexedSeq[A], v: A) = {
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
  protected def shrinkBinArrayByOne[T: Numeric](binArray: Array[Bin[T]], currentItemCount: Int): Unit = {
    /* finding the tuples to combine */
    assert(binArray.length > currentItemCount)

    /* PART 2 - finding the tuples to combine */
    var minDifference: T = binArray(0)._1 + 100000000 //TODO this would be better if this was infinity
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
        minDifference = implicitly[Numeric[T]].zero
      }
    })

    val combined = combineBins(binArray(minDiffIdx), binArray(minDiffIdx + 1))

    /* PART 3 - assigning and combining */
    binArray.update(minDiffIdx, combined)
    Range(minDiffIdx + 1, currentItemCount).foreach(idx => {
      binArray.update(idx, binArray(idx + 1))
    })

  }

  protected def combineBins[T: Numeric](b1: Bin[T], b2: Bin[T]): Bin[T] = {
    val (q1,k1) = b1
    val (q2,k2) = b2

    val k = k1 + k2
    if(k == 0) {
      //we don't want to divide by 0
      (implicitly[Numeric[T]].zero, 0)
    } else {
      val q = (q1 * k1 + q2 * k2) / (k1 + k2)
      (q, k)
    }
  }
}
