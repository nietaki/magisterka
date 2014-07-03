package test

import net.almost_done.data_processing.DataTransformer
import net.almost_done.trees._
import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Parameters, TraversableMatchers}
import org.specs2.mutable._
import spire.implicits._
import spire.math._

import scala.util.Random // provides infix operators, instances and conversions

/**
 * Created by nietaki on 03.07.14.
 */
class DataSpec extends Specification with TraversableMatchers with ScalaCheck {
  import Generators._
  "DataTransformer.sanitize" should {
    "remove leading and trailing spaces correctly" in {
      val in: String = " foobar "
      DataTransformer.sanitize(in) mustEqual("foobar")
    }

    "always pass" in prop { (i:Int, i2:Int) =>
      i + i2 == i2 + i
    }

    "leave no spaces at all" in Prop.forAllNoShrink(printableAsciiString){ s: String =>
      !DataTransformer.sanitize(s).contains(" ")
    }
  }

  "DataTransformer.sanitizeAndSplitRow" should {
    "split to the appropriate amount of partitions" in Prop.forAllNoShrink(printableAsciiString) { s: String =>
      val splitters = Set(':', ',')
      val partitions = DataTransformer.sanitizeAndSplitRow(s)
      if (s.length > 0 && splitters.contains(s.last)) {
        true
      } else {
        partitions.length == s.count(splitters.contains(_)) + 1
      }
    }

    "split in partitions not containing spaces" in Prop.forAllNoShrink(printableAsciiString) { s: String =>
      val partitions = DataTransformer.sanitizeAndSplitRow(s)
      partitions.forall(p => ! p.contains(' '))
    }
  }

}
