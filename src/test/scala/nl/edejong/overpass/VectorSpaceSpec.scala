package nl.edejong.overpass

import org.scalatest._

class VectorSpaceSpec  extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  "A VectorSpace" should "multiply" in {
    import VectorSpaceOps._
    2 |*| 3 should be (6)
    //2.|-| should be (-2)

    (2.0 lerp 3.0)(0.5) should be(2.5)
  }
}
