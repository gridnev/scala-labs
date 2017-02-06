package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
/**
 * In this Lab you will implement a ScalaTest testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a ScalaTest using a Spec of your choice to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class ScalaTestExerciseTest extends FunSuite{
  test("Happy flow (divider is > 0)") {
    val e2 = new Euro(2, 50) / 2

    assert(e2.euro == 1)
    assert(e2.cents == 25)
  }

  test("Alternative flow (divider is <= 0)") {
    intercept[IllegalArgumentException]{
      new Euro(2) / 0
    }
  }

}
