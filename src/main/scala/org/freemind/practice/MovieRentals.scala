package org.freemind.practice

import scala.collection.mutable.ListBuffer


object MovieRentals {

  /**
    * PriceRule
    */
  abstract trait PriceRule {
    def totalAmount(daysRented: Int): Double
    def frequentPoint(daysRented: Int): Int = 1
  }

  class DailyCharge(costPerDay: Double) extends PriceRule {
    override def totalAmount(daysRented: Int): Double =
      costPerDay * daysRented
  }
  case object NewRelease extends DailyCharge(3.0) {
    override def frequentPoint(daysRented: Int): Int =
      if (daysRented > 1) super.frequentPoint(daysRented) + 1 else super.frequentPoint(daysRented)
  }
  case object Classic extends DailyCharge(1.0)

  class FixedPlusAddtl(fixedAmt: Double, daysThreshold: Int, addtlAmtPerDay: Double) extends PriceRule {
    override def totalAmount(daysRented: Int): Double = {
      val daysExceeded = if (daysRented > daysThreshold) daysRented - daysThreshold else 0
      fixedAmt + daysExceeded * addtlAmtPerDay
    }
  }
  case object Regular extends FixedPlusAddtl(2.0, 2, 1.5)
  case object Children extends FixedPlusAddtl(1.5, 3, 1.5)

  /*
     Movie, Rental and Customer
   */
  case class Movie(title: String, price: PriceRule)
  case class Rental(movie: Movie, dayRented: Int)

  case class Customer(name: String) {
    val rentals = ListBuffer[Rental]()
    var freqPoints = 0

    def addRental(r: Rental): Unit =
      rentals += r

    def statement: String = {
      val (totalAmt, fp) = rentals.foldLeft((0.0: Double, 0: Int)){
        case ((amt, fp), r) =>
          val p = r.movie.price
          (amt + p.totalAmount(r.dayRented), fp + p.frequentPoint(r.dayRented))
      }
      freqPoints += fp
      rentals.clear()
      "\nRental record for %s\nAmount owed is %3.1f \nYou earned %d frequent renter points. \n ".format(name, totalAmt, fp)
    }
  }

  def main(args: Array[String]) = {
    val movie1 = Movie("Frozen", Children)
    val movie2 = Movie("Bourn Identity", Regular)
    val movie3 = Movie("Roma", NewRelease)
    val movie4 = Movie("Gone with Wind", Classic)
    val c1 = Customer("Wylie Burp")
    val c2 = Customer("Stephen Curry")
    val c3 = Customer("LeBrown James")
    val c4 = Customer("James Harden")
    val r1 = Rental(movie1, 2)
    val r2 = Rental(movie2, 3)
    val r3 = Rental(movie3, 2)
    val r4 = Rental(movie4, 2)

    c1.addRental(r1)
    print(c1.statement)
    c2.addRental(r2)
    print(c2.statement)
    c3.addRental(r3)
    print(c3.statement)
    c4.addRental(r4)
    print(c4.statement)

    c1.addRental(r1)
    c1.addRental(r2)
    c1.addRental(r3)
    c1.addRental(r4)
    print(c1.statement)

  }


}
