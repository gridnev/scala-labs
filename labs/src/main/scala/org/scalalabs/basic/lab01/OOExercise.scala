package org.scalalabs.basic.lab01
import scala.language.implicitConversions
import org.scalalabs.basic.lab01.DefaultCurrencyConverter
/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 *
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 *
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 *   -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 *   -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method
 *
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Euro to Dollar using the
 *   [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */

abstract class Currency(val symbol:String)

object Euro {
  def apply(euro: Int, cents: Int): Euro = new Euro(euro, cents)

  def fromCents(cents: Int): Euro = Euro((cents - cents % 100) / 100, cents % 100)

  implicit class IntWithEuro(x: Int) {
    def *(euro: Euro): Euro = euro * x
  }
}

class Euro(val euro:Int, val cents:Int = 0) extends Currency("EUR") with Ordered[Euro] {
  def inCents: Int = euro * 100 + cents

  def +(that: Euro): Euro =
    Euro.fromCents(this.inCents + that.inCents)

  def *(m: Int): Euro = Euro.fromCents(this.inCents * m)

  override def toString: String = cents match {
    case 0 => symbol + ": " + euro + ",--"
    case _ => symbol + ": " + euro + ',' + "%02d".format(cents)
  }

  override def compare(that: Euro): Int = {
    if (this.inCents < that.inCents) -1
    else if (this.inCents > that.inCents) 1
    else 0
  }
}

object Dollar{
  def apply(dollar: Int, cents: Int): Dollar = new Dollar(dollar, cents)

  def fromCents(cents: Int): Dollar = Dollar((cents - cents % 100) / 100, cents % 100)

  implicit def fromDollar(from: Dollar)(implicit currencyConverter: CurrencyConverter): Euro =
    Euro.fromCents(currencyConverter.toEuroCents(from.inCents))
}

class Dollar(dollar:Int, cents:Int) extends Currency("USD"){
  def inCents: Int = dollar * 100 + cents
}

