package net.almost_done.trees

/**
 * Created by nietaki on 3/7/14.
 */
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions
import scala.annotation.tailrec

/**
 */
case class AttributeHistogram[T: Numeric] private (bins: Vector[AttributeHistogram.Bin[T]]){
  import AttributeHistogram.Bin

  //var bins: Vector[Bin[T]] = Vector.fill(binCount + 1)(Tuple2((implicitly[Numeric[T]].zero), 0))

  lazy val tZero = implicitly[Numeric[T]].zero

  def observationCount: Int = bins.map(_._2).sum
  /** for convenience index of the last modified value */


  def binKeys = bins.map{pr => pr._1}.toSeq
  def binKeysAll = bins.map{pr => pr._1}.toSeq

  def weightedAverage: Double =
    if(observationCount == 0)
      0.0
    else
      bins.map{case (k, v) => k.toDouble()*v}.sum / observationCount.toDouble

  /**
   * @param newOccurrence attribute value to add to the histogram
   * @return the histogram with the new value inserted
   */
  def updated(newOccurrence: T): AttributeHistogram[T]= {
    /* PART 1 - insert */
    val updatingTuple = Tuple2(newOccurrence, 1)

    val binsWithNewValue = AttributeHistogram.withInsertedInOrder(bins, updatingTuple)
    new AttributeHistogram[T](AttributeHistogram.shrinkBinVectorByOne(binsWithNewValue))
  }


  def merged(other: AttributeHistogram[T]): AttributeHistogram[T]= {
    assert(this.bins.length == other.bins.length)
    val combinedBinsSorted = (this.bins ++ other.bins).toArray
    //FIXME - better sort
    Sorting.mergeSort(combinedBinsSorted)
    val sortedVector = combinedBinsSorted.toVector
    val result = Range(sortedVector.length, this.bins.length, -1).foldLeft(sortedVector)((ahVector, size) => AttributeHistogram.shrinkBinVectorByOne(ahVector))
    new AttributeHistogram(result)
  }

  /**
   *
   * @param b a point b such that p_1 < b < b_B
   * @return Estimated number of points in the interval [-\infty, b]
   */
  def sum(b: T): Double = {
    val i = AttributeHistogram.binarySearch(this.bins, (b, 0))
    val bDouble = b.toDouble() //FIXME make sure we need to convert to double
    //TODO: replace arr with a lazy view with prepended and appended bins
    def doublify(b: Bin[T]): (Double, Double) ={
      (b._1.toDouble(), b._2.toDouble)
    }
    val (pi, mi) = doublify(bins(i))
    val (pi2, mi2) = doublify(bins(i+1))

    //TODO analyze the situation for when it's (either) the end of the array

    val mb: Double = mi+ (mi2 - mi)/ (pi2 - pi) * (bDouble - pi)
    val s: Double = (mi + mb) / 2 * (bDouble - pi)/(pi2 - pi)
    //the view does exclude the i-th bin
    val partialSum = bins.view(0, i).foldLeft(0){case (rollingSum, pr)  => rollingSum + pr._2}
    s + partialSum
  }

  override def toString = bins.toList.toString
}

object AttributeHistogram {
  type Bin[T] = (T, Int)

  def empty[T: Numeric](binCount: Int) = new AttributeHistogram[T](Vector.fill(binCount)(Tuple2((implicitly[Numeric[T]].zero), 0)))

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

  def withInsertedInOrder[T: Order](v: Vector[T], t: T): Vector[T] = {
    val pos = binarySearch(v, t)
    val (beg, end) = v.splitAt(pos)
    (beg :+ t) ++ end
  }
   /**
   * merges the appropriate bins together, effectively reducing binArray's size by 1
   * @param binArray the binArray the method should be modifying
   * @param currentItemCount currentItemCount. The Array should be bigger than currentItemCount by at least 1.
   */
  def shrinkBinArrayByOne[T: Numeric](binArray: Array[Bin[T]], currentItemCount: Int): Unit = {
    /* finding the tuples to combine */
    assert(binArray.length > currentItemCount)

    /* PART 2 - finding the tuples to combine */
    var minDifference: Option[T] = None
    var minDiffIdx: Int = 0;
    Range(0, currentItemCount).foreach(idx => {
      val diff = binArray(idx+1)._1 - binArray(idx)._1
      //if(diff < minDifference) {
      if(minDifference.map(diff < _).getOrElse(true)) {
        minDiffIdx = idx
        minDifference = Some(diff)
      }

      if(binArray(idx)._2 == 0) {
        //the bin is empty, we can replace it no worries
        minDiffIdx = idx
        minDifference = Some(implicitly[Numeric[T]].zero)
      }
    })

    val combined = combineBins(binArray(minDiffIdx), binArray(minDiffIdx + 1))

    /* PART 3 - assigning and combining */
    binArray.update(minDiffIdx, combined)
    Range(minDiffIdx + 1, currentItemCount).foreach(idx => {
      binArray.update(idx, binArray(idx + 1))
    })

  }

  /**
   * an updated version of shrinkBinArrayByOne
   * @param binVector
   * @tparam T
   * @return
   */
  def shrinkBinVectorByOne[T: Numeric](binVector: Vector[Bin[T]]): Vector[Bin[T]] = {
    assert(binVector.length >= 2)

    def diffCalculator(b1: Bin[T], b2: Bin[T]): T = {
      if (b1._2 == 0 || b2._2 == 0){
        implicitly[Numeric[T]].zero
      } else {
        b2._1 - b1._1
      }
    }

    val diffsWithStartingBinIndices = binVector.sliding(2).map({ window =>
      val b1 =  window.head
      val b2 =  window.last
      diffCalculator(b1, b2)
    }).zipWithIndex

    //this chooses the smallest diff, taking into account the empty bins
    val diffAndIndexToBeMerged = diffsWithStartingBinIndices.reduce(min(_, _))
    val targetIndex = diffAndIndexToBeMerged._2
    val combinedBins = combineBins(binVector(targetIndex), binVector(targetIndex+1))
    (binVector.take(targetIndex) :+ combinedBins) ++ binVector.drop(targetIndex+2)
    //TODO TEST THIS
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
