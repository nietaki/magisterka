package test

import spire.algebra._
import spire.math._
import spire.implicits._

/**
 * Created by nietaki on 04.07.14.
 */
object MatcherHelpers {
  def isSorted[T: Order](seq: Seq[T]) = {
    (seq.length < 2) || seq.sliding(2).forall(w => w.head <= w.last)
  }
}
