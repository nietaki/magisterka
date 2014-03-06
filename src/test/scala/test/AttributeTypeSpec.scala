package test

import org.specs2.mutable._

import net.almost_done.trees._

class AttributeTypeSpec extends Specification {
 
  "Ignore" should {
    "parse everything as None" in {
      Ignore.parseRepresentation("0") must be equalTo(None)
    }

    "accept any type nad visualise it as a question mark" in {
      Ignore.retrieveRepresentation(0) must beEqualTo("?")
    }
  }

  "ContinousDouble" should {
    "generate parseable representations of floats" in {
      val repr = ContinuousDouble.retrieveRepresentation(3.14159)
      repr.toDouble must be closeTo(3.14159, 0.0001)
    }

    "parse correctly formatted floats" in {
      ContinuousDouble.parseRepresentation("3.14159").get must be closeTo(3.14159, 0.00001)
      ContinuousDouble.parseRepresentation("3.14159E2").get must be closeTo(314.159, 0.00001)
    }

    "parse an '?' as a missing value" in {
      ContinuousDouble.parseRepresentation("?") mustEqual None
    }

    "throw an exception on malformed strings" in {
      ContinuousDouble.parseRepresentation("PI") should throwA[NumberFormatException]
    }


  }

  "Nominal" should {
    val n = Nominal(Array("zero", "one", "two", "three", "four"))
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
  }

}