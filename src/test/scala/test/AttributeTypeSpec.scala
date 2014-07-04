package test

import net.almost_done.data_processing.{AttributeTypeFactory, DataTransformer}
import net.almost_done.data_processing.attributes._
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable._

import net.almost_done.trees._

class AttributeTypeSpec extends Specification with ScalaCheck{
  def Ignore2 = Ignore("foo", 1)
  def ContinuousDouble2 = new ContinuousDouble("foo", 1)
  def contInt = new ContinuousInteger("foo", 13)
  "Ignore" should {
    "parse everything as None" in {
      Ignore2.parseRepresentation("0") must be equalTo(None)
    }

    "accept any type nad visualise it as a question mark" in {
      Ignore2.retrieveRepresentation(0) must beEqualTo("?")
    }
  }

  "ContinousDouble" should {
    "generate parseable representations of floats" in {
      val repr = ContinuousDouble2.retrieveRepresentation(3.14159)
      repr.toDouble must be closeTo(3.14159, 0.0001)
    }

    "parse correctly formatted floats" in {
      ContinuousDouble2.parseRepresentation("3.14159").get must be closeTo(3.14159, 0.00001)
      ContinuousDouble2.parseRepresentation("3.14159E2").get must be closeTo(314.159, 0.00001)
    }

    "parse all correctly formatted floats" in prop{ d: Double =>
      (ContinuousDouble2.parseRepresentation(d.toString()).get - d).abs < 0.0001
    }

    "parse an '?' as a missing value" in {
      ContinuousDouble2.parseRepresentation("?") mustEqual None
    }

    "throw an exception on malformed strings" in {
      ContinuousDouble2.parseRepresentation("PI") should throwA[NumberFormatException]
    }
  }
  "ContinuousInt" should {
    "be constructed from a row correctly" in {
      AttributeTypeFactory.attribute("foo", 13, Array("continuous-integer")).isInstanceOf[ContinuousInteger]
    }

    "parse all ints correctly" in prop {i: Int =>
      contInt.parseRepresentation(i.toString()) == Some(i)
    }
  }

  "Nominal" should {
    val n = new Nominal("foo",0, Array("zero", "one", "two", "three", "four"))
    "parse the given nominal values to correct integers" in {
      n.parseRepresentation("zero").get mustEqual 0
      n.parseRepresentation("one").get mustEqual 1
      n.parseRepresentation("two").get mustEqual 2
      n.parseRepresentation("three").get mustEqual 3
      n.parseRepresentation("four").get mustEqual 4
    }

    "parse '?' as None" in {
      n.parseRepresentation("?") mustEqual None
    }

    "generate the enumeration names correctly" in {
      n.retrieveRepresentation(0) mustEqual "zero"
      n.retrieveRepresentation(1) mustEqual "one"
      n.retrieveRepresentation(2) mustEqual "two"
      n.retrieveRepresentation(3) mustEqual "three"
      n.retrieveRepresentation(4) mustEqual "four"
    }

    "throw exception for incorrect nominal values" in {
      n.parseRepresentation("umpteen") should throwA[NoSuchElementException]
    }

    "always recognize the correct element of the sequence" in Prop.forAllNoShrink(Generators.printableAsciiString) {s: String =>
      val values = DataTransformer.sanitizeAndSplitRow(s).filter(! AttributeType.unknownStrings.contains(_))
      val idx = scala.util.Random.nextInt(values.length)
      val at = AttributeTypeFactory.intAttribute("foo", 13, values)

      at match {
        case n: Nominal => {
          n.parseRepresentation(values(idx)) == Some(idx)
        }
        case _ => false
      }
    }
  }

}