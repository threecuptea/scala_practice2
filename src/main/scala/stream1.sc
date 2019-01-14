

def streamer(v: Int): Stream[Int] = Stream.cons(v, streamer(v + 1))

lazy val streamer1 = streamer(2)

streamer1 take 3 toList

(streamer1 drop 6) take 3 toList

def isPrime(n: Int) = (2 to math.sqrt(n).toInt) forall(n % _ != 0)

isPrime(113)

//finds the second prime number between 1000 and 10000
(Stream.from(1000) filter isPrime)(1)

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
}

val primes2 = sieve(Stream.from(2))

primes2 take 100 toList

//How to I convert sieve to isPrime

lazy val fibz: Stream[Int] = 1 #:: 1 #:: fibz.zip(fibz.tail).map { n => n._1 + n._2 }

(fibz take 5 toList)
(fibz take 6 toList)

lazy val fibs: Stream[Int] = {
  def loop (h: Int, n: Int): Stream[Int] = h #:: loop(n, h + n)
  loop(1, 1)
}

(fibs take 5 toList)
(fibs take 6 toList)


































