package test

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.TraversableMatchers
import org.specs2.mutable._

/**
 * Created by nietaki on 04.07.14.
 */
class ExperimentalSpec extends Specification with TraversableMatchers with ScalaCheck  {
  "AttributeHistogram generator" should {
    "generate something" in Prop.forAll(Generators.alphaNumOrSpace) { ah => true }
    "generate an Int AttributeHistogram" in Prop.forAll(Generators.attributeHistogramOfSize[Int](10)) { ah => true }
    "generate a Double AttributeHistogram" in Prop.forAll(Generators.attributeHistogramOfSize[Double](10)) { ah => true }

    "create tests without explicit generators" in prop { (i:Int, i2: Int) =>
      i < i2 || i >= i2
    }

    "have list.sliding the same length as list" in prop {(l: List[Int]) =>
      l.sliding(2).length == l.length
    }
  }

}
