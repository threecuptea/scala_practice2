/**
  * High-order function and Stream
  */

import scala.annotation.tailrec

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(1001, 35)

def isPrime(n: Int) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)

isPrime(1001)
isPrime(1013)
isPrime(Integer.MAX_VALUE)


def isPrimeLong(n: Long) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)

//val now = System.currentTimeMillis()
//isPrimeLong(8914393174836497l)
//System.currentTimeMillis() - now

//The second prime from 1000 to 1000000
Stream.from(1000).filter(isPrime) (1)
//isPrime can be used for checking a specific number or to generate prime list up to
Stream.from(2).filter(isPrime) takeWhile(_ < 100) toList


def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
}
//it module all heads. Head are different in each nested inner sieve call.
/*
3 module 2
4 module 2
5 module 2
5 module 3
6 module 2
7 module 2
7 module 3
7 module 5
 */

lazy val primeSieve = sieve(Stream.from(2))
(primeSieve takeWhile(_ < 100) toList)
(primeSieve take 100 toList)
//lazy val will memorize what has been saved, It won't repeat the same process

//I cannot use from(1000), otherwise head will start with 1000 and skip % 2, %3.  That's why I got 1000, 1001
primeSieve.dropWhile(_ < 1000)(1)

def isPrime2(n: Int): Boolean = {
  val stream = primeSieve.takeWhile(_ <= n)
  stream(stream.size -1) == n
}

isPrime2(1001)
isPrime2(1013)
//sieve is too slow for a large number
//isPrime2(Integer.MAX_VALUE)

def sieve2(s: Stream[Int]): Stream[Int] = {
  s.head #:: (sieve2(s.tail.filter{ x=>
    println("%d module %d".format(x, s.head))
    x % s.head != 0
  }
  ))
}

lazy val primeSieve2 = sieve2(Stream.from(2))
(primeSieve2 take 4 toList)
(primeSieve2 take 5 toList)
//lazy val will memorize what has been saved, it won't repeat 2,3,4,5,6,7, it will starts with 8,9, 10

lazy val fibz: Stream[Int] = 1 #:: 1 #:: fibz.zip(fibz.tail).map(n =>  n._1 + n._2)

val now_fibz = System.currentTimeMillis()
(fibz take 25 toList)
System.currentTimeMillis() - now_fibz

lazy val fibs: Stream[Int] = {
  def loop(h: Int, n: Int): Stream[Int] = h #:: loop(n, n + h)
  loop(1,1)
}

//The followings is faster for long list
val now_fibs = System.currentTimeMillis()
(fibs take 25 toList)
System.currentTimeMillis() - now_fibs


def factorial(n: Int): Int = {
  if (n == 1) 1 else n * factorial(n - 1)
}

factorial(5)

def factorial_tail(n: Int): Int = {
  @tailrec
  def loop(i: Int, result: Int): Int =
    if (i == 1) result else loop(i-1, i * result)

  loop(n, 1)
}

factorial_tail(5)

def sum(f: Int => Int, a: Int, b: Int): Int = {
  //Need a > not a = b otherwise will miss the last number
  if (a > b) 0 else f(a) + sum(f, a + 1, b)
}

def sumInts(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
def sumFactorials(a: Int, b: Int) = sum(factorial, a, b)

sumInts(1, 5)
sumCubes(1, 4)
sumFactorials(1, 4)

def sum_tail(f: Int => Int, a: Int, b: Int): Int = {
  def loop(f: Int => Int, x: Int, y: Int, result: Int): Int =
    if (x > y) result else loop(f, x + 1, y, f(x) + result)

  loop(f, a, b, 0)
}

sum_tail(x => x, 1, 5)
sum_tail(x => x * x * x, 1, 4)
sum_tail(factorial, 1, 4)


val chars = 'a' to 'z'

//Will get Vector in default
{for {
  a <- chars
  b <- chars if b > a
} yield "%c%c".format(a, b)}.toList


val nums = (1 to 5)

{for {
  x <- nums
  y <- nums if y > x
} yield (x, y)}.toList



def average(x: Int, xs: Int*): Double = {
  (x :: xs.toList).sum.toDouble / (xs.size + 1)
}

average(1)
average(1, 2)
average(1, 2, 3)

val lst = List(2, 3, 4)
average(1, lst: _*)

type Result = Either[String, (Int, Int)]

def divide(numer: Int, denom: Int): Result = denom match  {
  case 0 => Left("Division by zero")
  case _ => Right(numer / denom, numer % denom)
}

divide(6, 4)
divide(2, 0)
divide(8, 4)




