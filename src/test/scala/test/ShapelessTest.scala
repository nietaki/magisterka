package test

import net.almost_done.data_processing.attributes.{ContinuousDouble, Ignore, Nominal}
import org.specs2.mutable._
import shapeless._
import poly._


class ShapelessTest extends Specification {
  "HList" should {
    "map in a trivial case" in {
      object ToOption extends (Id ~> Option) {
        def apply[T](t: T) = Some(t)
      }

      val h0 = 1.0 :: "two" :: HNil
      val h1 = Some(1.0) :: Some("two") :: HNil
      val hMapped = h0.map(ToOption)
      val zipped = h1.zip(hMapped)


      object comparePairs extends Poly1 {
        implicit def caseTuple[T, U] = at[(T, U)](t => t._1 == t._2)
      }
      val same = zipped.map(comparePairs)
      true must beTrue
      same(0) must beTrue
      same(1) must beTrue
      val zipped2 = h0.zip(h1).map(comparePairs)
      zipped2(0) must beFalse
    }
  }
}