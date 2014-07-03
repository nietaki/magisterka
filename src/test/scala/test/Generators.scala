package test

import org.scalacheck.Gen

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
  def printableAsciiString: Gen[String] = stringGen(printableAsciiChar)

  def alphaNumOrSpace: Gen[Char] = Gen.frequency((9, Gen.alphaNumChar), (1, Gen.value(' ')))
  def alphaNumOrSpaceString = stringGen(alphaNumOrSpace)
}
