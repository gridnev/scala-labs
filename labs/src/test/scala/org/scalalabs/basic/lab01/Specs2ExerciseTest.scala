package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
/**
 * In this Lab you will implement a Specs2 testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a Specs2 specification to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class Specs2ExerciseTest extends Specification {
  "def /(divider:Int)" should {
    "Happy flow (divider is > 0)" in {
      val e = new Euro(2, 50) / 2
      e.euro ==== 1
      e.cents ==== 25
    }

    "Alternative flow (divider is <= 0)" in {
      new Euro(2, 50) / 0 must throwA[IllegalArgumentException]
    }
  }
}
