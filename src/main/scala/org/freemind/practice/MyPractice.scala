package org.freemind.practice

import scala.annotation.tailrec
import scala.collection.immutable.Stream.cons
import scala.collection.mutable.ListBuffer


case class Dog(first: String, last: String)
object MyPractice {

  def main(args: Array[String]): Unit = {

    val d1 = Dog("Scooby", "Doberman")
    println()
    println(d1.toString)

    val stream1: Stream[Int] = 1 #:: 2 #:: 3 #:: Stream.empty[Int]
    println(s"Element of stream1: $stream1")

    val stream2: Stream[Int] = cons(1, cons(2, cons(3, Stream.empty[Int])))
    println(s"Element of stream2: $stream2")

    println("stream2 take 3")
    stream2.take(3).print()
    print("\nstream2 take 10\n")
    stream2.take(10).print()

    def inf(num: Int): Stream[Int] = cons(num, inf(num + 1))

    print("\ninf take 20\n")
    inf(1) take 20 print

    val stream3: Stream[Int] = Stream.from(1)
    print("\nfrom 1 take 20\n")
    stream3 take 20 print


    val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    print("\ndropWhile odds\n")
    println(numbers.dropWhile(_ % 2 != 0)) // think the opposite of takeWhile
    println("partition by even")
    println(numbers.partition(_ % 2 == 0))
    println("find")
    println(numbers.find(_ % 2 == 0))

    //Given a Traversable (x1, x2, x3), an initial value of init, an operation op, foldLeft is defined as: ((init op x1) op x2) op x3
    println("foldLeft") //m is accumulator, n is addition starting from the left: 1, 2,, 3....
    println(numbers.foldLeft(0) { (m: Int, n: Int) =>
      println("m = " + m + " + n = " + n)
      m + n
    })
    //println(numbers.foldLeft(0)(_ + _))


    //Given a Traversable (x1, x2, x3), an initial value of init, an operation op, foldRight is defined as: x1 op (x2 op (x3 op init))
    println("foldRight") //n is accumulator, m is addition starting from the right: 10, 9, 8, 7
    println(numbers.foldRight(0) { (m: Int, n: Int) =>
      println("m = " + m + " + n = " + n)
      m + n
    })
    //println(numbers.foldRight(0)(_ + _))

    val chars = 'a' to 'z'

    val perms = (for {
      a <- chars
      b <- chars
      if a != b
    } yield "%c%c".format(a, b)).toList

    println(perms)

    val nestedNumbers = List(List(1, 2), List(3, 4))
    println("flatMap")
    println(nestedNumbers.flatMap(x => x.map(_ * 2)))

    def timesTwo(i: Int): Int = i * 2

    println("ourMap using foldRight")

    def ourMap(nums: List[Int], f: Int => Int): List[Int] = {
      nums.foldRight(List[Int]()) {
        (x: Int, xs: List[Int]) => f(x) :: xs
      }
    }

    println(ourMap(numbers, timesTwo(_)))

    def isEven(x: Int) = x % 2 == 0

    println("isEven filter")
    println(numbers filter isEven)

    //splitAt will split a Traversable at a position, returning a 2 product Tuple,
    //splitAt is also defined as (xs take n, xs drop n)
    val arr1 = Array(87, 44, 5, 4, 200, 10, 39, 100)
    val result = arr1.splitAt(3) //Return a tuple of Array

    println(result._1.toList)
    println(result._2.toList)

    val builder = new StringBuilder

    val x = {
      builder += 'x'; 1
    }
    lazy val y = {
      builder += 'y'; 2
    }

    def z = {
      builder += 'z'; 3
    }

    println()
    println(builder.result) //x
    println(z + y + x + z + y + x) //12, 2 values
    println(builder.result) //xzyz (compile, run, run (first only), run

    lazy val fibz: Stream[Int] = 1 #:: 1 #:: fibz.zip(fibz.tail).map { n =>
      println("Adding %d and %d".format(n._1, n._2))
      n._1 + n._2
    }

    println()
    fibz take 5 foreach println
    println()
    fibz take 6 foreach println

    println()

    def isPrime(n: Int) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)
    def isPrimeLong(n: Long) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)


    println()
    println("isPrime: The second prime number starting from 1000: %d".format((Stream.from(1000) filter isPrime) (1)))


    var start = System.currentTimeMillis
    println("isPrime 2147483647 %s".format(isPrime(Integer.MAX_VALUE)))
    println("it takes %d".format( System.currentTimeMillis - start ))

    start = System.currentTimeMillis
    println("isPrime 8914393174836497 %s".format(isPrimeLong(8914393174836497l)))
    println("it takes %d".format( System.currentTimeMillis - start ))


    println()
    def sieve(s: Stream[Int]): Stream[Int] = {
      //println("head: %d".format(s.head))
      s.head #:: sieve(s.tail.filter { x =>
        //println("%d module %d".format(x, s.head))
        x % s.head != 0
      })
    }
    //It is still not ideal because it module all prime.  It should only module up to

    lazy val primeSieve = sieve(Stream.from(2))

    println(primeSieve take 5 toList)
    //println() //lazy shorten the time
    //prime2 take 6 foreach println


    def isPrimeSieve(n: Int): Boolean = {
      var i = 1
      var done = false
      var saved = 0
      while (!done) {
        saved = (primeSieve take i) (i - 1)
        if (saved >= n)
          done = true
        else
          i += 1
      }
      if (saved == n)
        true
      else
        false
    }
    println()
    //println("isPrimeSieve: Is 19 prime: %s".format(isPrimeSieve(19)))
    //println("isPrimeSieve: Is 541 prime: %s".format(isPrimeSieve(541)))

    //println()
   // println("isPrimeSieve: The second prime number starting from 1000: %d".format((Stream.from(1000) filter isPrimeSieve) (1)))

    //start = System.currentTimeMillis
    //println("isPrimeSieve 2147483647 %s".format(isPrimeSieve(Integer.MAX_VALUE)))
    //println("it takes %d".format( System.currentTimeMillis - start ))

    def primeSieveUnder(n: Int, incl: Boolean = false): List[Int] = {
      var i = 1
      var done = false
      while (!done) {
        val last = (primeSieve take i) (i - 1)
        if (last < n) {
          i += 1
        }
        else {
          done = true
          if (last == n && incl)
            i += 1
        }
      }
      primeSieve take (i-1) toList
    }

    //println()
    //println(primeSieveUnder(100))
    start = System.currentTimeMillis
    primeSieveUnder(10000)
    println("primeSieveUnder: it takes %d".format( System.currentTimeMillis - start ))


    def primesUnder(n: Int): List[Int] = {
      require(n > 2)

      val primes = ListBuffer(2)
      for (i <- 3 until n) {
        if (prime(i, primes.iterator))
          primes += i
      }
      primes.toList
    }

    start = System.currentTimeMillis
    primesUnder(10000) //This is more efficient than Sieve
    println("primesUnder: it takes %d".format( System.currentTimeMillis - start ))

    def generatePrimeListUptoSquareRoot(n: Long): ListBuffer[Int] = {
      require(n > 2)

      val primes = ListBuffer(2)
      for (i <- 3 to math.sqrt(n).toInt) {
        if (prime(i, primes.iterator))
          primes += i
      }
      primes
    }

    def prime(i: Int, it: Iterator[Int]): Boolean = {
      it.takeWhile(_ <= Math.sqrt(i).toInt).forall(i % _ != 0)
    }

    def primeReverse(i: Int, it: Iterator[Int]): Boolean = {
      it.dropWhile(_ > Math.sqrt(i).toInt).forall(i % _ != 0)
    }

    def primeLong(n: Long, given: Iterator[Int]): Boolean = {
      given.forall(n % _ != 0)
    }

    start = System.currentTimeMillis
    val lb:ListBuffer[Int] = generatePrimeListUptoSquareRoot(Integer.MAX_VALUE)
    println("primeLong 2147483647 %s".format(primeLong(Integer.MAX_VALUE, lb.iterator)))
    println("it takes %d".format( System.currentTimeMillis - start ))

    //It is slow.  It take time to generate list
    //start = System.currentTimeMillis
    //val lb2:ListBuffer[Int] = generatePrimeListUptoSquareRoot(8914393174836497l)
    //println("isPrime 8914393174836497l %s".format(primeLong(8914393174836497l, lb2.iterator)))
    //println("it takes %d".format( System.currentTimeMillis - start ))

    def primesUnder2(n: Int): List[Int] = {
      require(n > 2)

      def rec(i: Int, primes: List[Int]): List[Int] = {
        if (i >= n)
          primes
        else
          if (primeReverse(i, primes.iterator))
            rec(i+1, i :: primes)
          else
            rec(i+1, primes)
      }

      rec(2, List())

    }

    start = System.currentTimeMillis
    primesUnder2(10000)
    println("primesUnder2: it takes %d".format( System.currentTimeMillis - start ))

    //Conclusion: generate prime list for 100000, sieve is aweful when large number involved
    //primeSieveUnder: it takes 51449
    //primesUnder: it takes 36
    //primesUnder2: it takes 5395 (no reverse)

    //Conclusion: isPrime is the best when very very large number involved.
    //It might be inefficient to test 4,6, 8, 9.  However, It might take more time to generate prime list when very large number is involved
    //Both are compatible when the number is very large not too large.  Sieve is aweful even for number 2147483647
    //when I generate prime list to test primeLong, I generatePrimeList only UptoSquareRoot.  That's O.K. to 2147483647 (prime number is about 10K)
    //It won't work fro 8914393174836497. Its prime list is 10M+
    //isPrime 2147483647 true
    //it takes 17
    //isPrime 8914393174836497 true
    //it takes 3658
    //primeLong 2147483647 true
    //it takes 14

  }




}
