package test

import org.specs2.mutable._

import net.almost_done.trees._

class AttributeTypeSpec extends Specification {
 
  "Ignore" should {
    "parse everything as None" in {
      Ignore.parseStringRepresentation("0") must be equalTo(None)
    }

    "accept any type nad visualise it as a question mark" in {
      Ignore.retrieveStringRepresentation(0) must beEqualTo("?")
    }
  }

  "ContinousDouble" should {
    "generate parseable representations of floats" in {
      val repr = ContinuousDouble.retrieveStringRepresentation(3.14159)
      repr.toDouble must be closeTo(3.14159, 0.0001)
    }

    "parse correctly formatted floats" in {
      ContinuousDouble.parseStringRepresentation("3.14159").get must be closeTo(3.14159, 0.00001)
      ContinuousDouble.parseStringRepresentation("3.14159E2").get must be closeTo(314.159, 0.00001)
    }

    "parse an '?' as a missing value" in {
      ContinuousDouble.parseStringRepresentation("?") mustEqual None
    }

    "throw an exception on malformed strings" in {
      pending("for now just returning None")
      ContinuousDouble.parseStringRepresentation("PI") should throwA[NumberFormatException]
    }


  }

  "Nominal" should {
    val n = Nominal(Array("zero", "one", "two", "three", "four"))
    "parse the given nominal values to correct integers" in {
      n.parseStringRepresentation("zero").get mustEqual 0
      n.parseStringRepresentation("one").get mustEqual 1
      n.parseStringRepresentation("two").get mustEqual 2
      n.parseStringRepresentation("three").get mustEqual 3
      n.parseStringRepresentation("four").get mustEqual 4
    }

    "parse '?' as None" in {
      n.parseStringRepresentation("?") mustEqual None
    }

    "generate the enumeration names correctly" in {
      n.retrieveStringRepresentation(0) mustEqual "zero"
      n.retrieveStringRepresentation(1) mustEqual "one"
      n.retrieveStringRepresentation(2) mustEqual "two"
      n.retrieveStringRepresentation(3) mustEqual "three"
      n.retrieveStringRepresentation(4) mustEqual "four"
    }

    "throw exception for incorrect nominal values" in {
      pending
      n.parseStringRepresentation("umpteen") should throwA[NoSuchElementException]
    }
  }

}