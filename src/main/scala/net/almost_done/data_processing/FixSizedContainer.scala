package net.almost_done.data_processing

/**
 * Created by nietaki on 3/7/14.
 */
case class FixSizedContainer[T, S <: Sizable](val values: Vector[T], s: S) {
  assert(values.length == s.getSize)

  def mate(other: FixSizedContainer[T, S]): FixSizedContainer[T, S] = {
    ??? //some code that makes sense here
  }
}

object FixSizedContainer {
  def size(i: Int): Sizable = ???
}

trait Sizable {
  def getSize: Int
}

case class ZeroSize(sth:Any) extends Sizable { //this should be an object but then I have an "not found: type ZeroSize" error
  val getSize = 0
}

case class SizeDesignator[S <: Sizable](oneLess: S) extends Sizable {
  def getSize: Int = oneLess.getSize + 1
}
