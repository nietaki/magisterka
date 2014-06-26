package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 25.06.14.
 *
 * wrapper base class for attribute values to make working on Int and Double histograms easier
 */
trait AttributeValue {
  def intVal: Option[Int];
  def doubleVal: Option[Double];
}

object AttributeValue {
  def apply[N: Numeric](v: Option[N]) = new AttributeValue {
    import Numeric.Implicits._ //TODO switch to spire Numeric here
    def intVal = v.map(_.toInt())
    def doubleVal = v.map(_.toDouble())
  }
}

