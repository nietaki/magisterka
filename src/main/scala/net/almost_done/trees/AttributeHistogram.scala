package net.almost_done.trees

/**
 * Created by nietaki on 3/7/14.
 */

import Ordering.Implicits._
import Numeric.Implicits._

/**
 * a *mutable* AttributeHistogram class this will make it much more performance-friendly
 * @param arr the mutable Array size of which determins the binCount
 * @param n the numeric extension class for T
 * @tparam T numeric type of the attribute, most commonly an Int or a Double
 */
class AttributeHistogram[T](arr: Array[(T, Int)])(implicit n: Numeric[T]){
  require(arr.sliding(2).forall(vec => {
    vec.head._1 <= vec.last._1
  }))


  /**
   * bin count
   */
  lazy val binCount = arr.length

  def update(newOccurrence: T) = {
    
  }
  def merge = ???
  def sum = ???


}

object AttributeHistogram {
  import Numeric.Implicits._
  import Ordering.Implicits._

  val ZeroObject = ???

}
