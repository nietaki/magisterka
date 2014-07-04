package test

import net.almost_done.trees.AttributeHistogram
import org.scalacheck.{Choose, Arbitrary, Gen}

import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions


/**
 * Created by nietaki on 03.07.14.
 */
object Generators {

  val smallUnsignedInt = Gen.choose[Int](1, 100)
  val smallInt = Gen.choose[Int](-100, 100)
  val smallIntTupleList:Gen[(List[Int], List[Int])] = {
    for{
      length <- smallUnsignedInt
      la <- Gen.listOfN(length, smallInt)
      lb <- Gen.listOfN(length, smallInt)
    } yield (la, lb)
  } // Dem Monads

  val sameTupleList:Gen[(List[Int], List[Int])] = {
    for{
      length <- smallUnsignedInt
      la <- Gen.listOfN(length, smallInt)
    } yield (la, la)
  }


  def stringGen(c: Gen[Char]): Gen[String] = for(ls <- Gen.listOf(c)) yield ls.mkString

  def printableAsciiChar: Gen[Char] = Gen.choose(32, 126).map(_.toChar)
  def printableAsciiCharWithoutQuestionMarks: Gen[Char] = Gen.choose(32, 126).map(_.toChar).filter(_ != '?')
  def printableAsciiString: Gen[String] = stringGen(printableAsciiChar)
  def printableAsciiStringWithoutQuestionMarks: Gen[String] = stringGen(printableAsciiCharWithoutQuestionMarks)

  def alphaNumOrSpace: Gen[Char] = Gen.frequency((9, Gen.alphaNumChar), (1, Gen.value(' ')))
  def alphaNumOrSpaceString = stringGen(alphaNumOrSpace)

  val maxAHMagnitude = 1000000 //TODO make this bigger

  @deprecated("use more generic AttributeHistogram generator", "")
  def attributeHistogram: Gen[AttributeHistogram[Int]] = for {
    bc <- Gen.choose[Int](1, 10)
    ls <- Gen.listOf(smallInt)
  } yield {
    val ah = AttributeHistogram.empty[Int](bc)
    ls.foreach(ah.update(_))
    ah
  }

  @deprecated("use more generic AttributeHistogram generator", "")
  def attributeHistogramPair: Gen[(AttributeHistogram[Int], AttributeHistogram[Int])] = for {
    bc <- Gen.choose[Int](2, 10)
    ls <- Gen.listOf(smallInt)
    ls2 <- Gen.listOf(smallInt)
  } yield {
    val ah = AttributeHistogram.empty[Int](bc)
    val ah2 = AttributeHistogram.empty[Int](bc)
    ls.foreach(ah.update(_))
    ls2.foreach(ah2.update(_))
    (ah, ah2)
  }


  def smallNumeric[T: Numeric: Arbitrary: Choose]: Gen[T] = {
    val numeric = implicitly[Numeric[T]]
    Gen.choose[T](numeric.fromInt(-maxAHMagnitude), numeric.fromInt(maxAHMagnitude))
  }
  def smallNumericList[T: Numeric: Arbitrary: Choose]: Gen[List[T]] = {
    Gen.listOf(smallNumeric[T])
  }

  def attributeHistogramOfSize[T: Numeric: Arbitrary: Choose](size: Int): Gen[AttributeHistogram[T]] = {
    smallNumericList.map( ls =>
      ls.foldLeft(AttributeHistogram.empty[T](size))(_.update(_))
    )
  }

  val intAH = attributeHistogramOfSize[Int](7)
  val doubleAH = attributeHistogramOfSize[Double](7)
}
