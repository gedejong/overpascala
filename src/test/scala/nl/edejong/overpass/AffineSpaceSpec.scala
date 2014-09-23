package nl.edejong.overpass

import org.joda.time.DateTime
import org.scalatest._

class AffineSpaceSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  "A AffineSpace" should "do linear interpolation" in {
    import AffineSpaceOps._
    import AffineSpace._
    import VectorSpace._

    val twelveOClock = new DateTime(2014, 1, 1, 12, 00)
    val oneOClock = new DateTime(2014, 1, 1, 13, 00)
    val twoOClock = new DateTime(2014, 1, 1, 14, 00)
    // Halfway between 12 o'clock and 2 o'clock, it should be 1 o'clock
    (twelveOClock alerp twoOClock)(0.5) should be (oneOClock)

    ((twelveOClock, 1.0d) alerp (twoOClock, 2.0d))(0.5) should be (oneOClock, 1.5)
  }
}
