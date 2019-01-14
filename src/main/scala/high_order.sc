import scala.annotation.tailrec

def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b,  a % b)

gcd(1001, 35)


lazy val fibs: Stream[Int] = {
  def loop(h: Int, n: Int):  Stream[Int] = h #:: loop(n, h + n)
  loop(1,1)
}

fibs take 10 toList

def isPrime(n: Int): Boolean = (2 to math.sqrt(n).toInt).forall(n % _ != 0)

isPrime(1001)
isPrime(1013)
isPrime(Integer.MAX_VALUE)


def factorial(n: Int): Int =
  if (n == 1) 1 else n * factorial(n -1)

factorial(5)

def factorial_tail(n: Int): Int = {
  @tailrec
  def loop(i: Int, result: Int): Int =
    if (i == 1) result else loop(i - 1, result * i)
  loop(n, 1)
}

factorial_tail(6)


def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)


def id(x: Int) = x
def cube(x: Int) = x * x * x

def sumInts(a: Int, b: Int) = sum(id, a, b)
def sumCubes(a: Int, b: Int) = sum(cube, a, b)
def sumFactorials(a: Int, b: Int) = sum(factorial, a, b)

def sumInts_a(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes_a(a: Int, b: Int) = sum(x => x * x * x, a, b)


sumInts(1, 5)
sumCubes(1, 4)
sumFactorials(1, 4)
sumInts_a(1, 5)
sumCubes_a(1, 4)


def sum_tail(f: Int => Int, a: Int, b: Int): Int = {
  @tailrec
  def loop(f: Int => Int, a: Int, b: Int, result: Int): Int = {
    if (a > b) result else loop(f, a + 1, b, f(a) + result)
  }
  loop(f, a, b, 0)
}
sum_tail(x => x, 1, 5)
sum_tail(x => x * x * x, 1, 4)
sum_tail(factorial, 1, 4)


val xs = List(1,2,3,4,5)
val ys = List(2,2,2,2,2)

xs.map(_+1)
for (x <- xs) yield x + 1

xs.filter(_ % 2 == 0)
for (x <- xs if x % 2 == 0) yield x

xs.filter(_ % 2 == 0).map(_ + 1)
for (x <- xs if x % 2 == 0) yield x + 1

xs.flatMap(x => ys.map(y => (x, y)))

for (x <- xs; y <- ys) yield (x, y)

for {
  x <- xs if x % 2 == 0
  y <- ys
} yield (x, y)

xs.filter(_ % 2 == 0).flatMap(x => ys.map(y => (x, y)))

def average(x: Int, xs: Int*): Double =
  (x :: xs.toList).sum.toDouble / (xs.size + 1)


average(1)
average(1, 2)
average(1, 2, 3)

val lst = List(2, 3, 4)
average(1, lst: _*)

type Result = Either[String, (Int, Int)]

def divide(dividend: Int, divisor: Int): Result =
  if (divisor == 0 ) Left("Division by zero")
  else Right(dividend / divisor, dividend % divisor)

divide(6, 4)
divide(2, 0)
divide(8, 4)


val array = Array(87, 44, 5, 4, 200, 10, 39, 100)
array take 3
array drop 3
val split = array splitAt 3
split._1
split._2

array.takeWhile(_ < 100)
array.dropWhile(_ < 100)
val span = array.span(_ < 100)
span._1
span._2

array.filter(_ < 100)
array.filterNot(_ < 100)
val p = array.partition(_ < 100)
p._1
p._2

array.forall(_ < 100)
array.exists(_ < 100)












































