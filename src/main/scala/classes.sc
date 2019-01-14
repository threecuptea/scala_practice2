
abstract class Set[A] {
  def incl(x: A)(implicit ord: Ordering[A]): Set[A]
  def contains(x: A)(implicit ord: Ordering[A]): Boolean
}

class NonEmpty[A](elem: A, left: Set[A], right: Set[A]) extends Set[A] {
  def incl(x: A)(implicit ord: Ordering[A]): Set[A] = {
    if (ord.lt(x, elem))
      new NonEmpty[A](elem, left.incl(x), right)
    else if (ord.gt(x, elem))
      new NonEmpty[A](elem, left, right.incl(x))
    else
      this
  }

  def contains(x: A)(implicit ord: Ordering[A]): Boolean = {
    if (ord.lt(x, elem))
      left.contains(x)
    else if (ord.gt(x, elem))
      right.contains(x)
    else
      true
  }
}

class Empty[A] extends Set[A] {
  def incl(x: A)(implicit ord: Ordering[A]): Set[A] =  new NonEmpty[A](x, new Empty[A], new Empty[A])
  def contains(x: A)(implicit ord: Ordering[A]): Boolean = false

}

def singleton[A](elem: A) = new NonEmpty[A](elem, new Empty[A](), new Empty[A]())

val singletonInt = singleton(2)

val tBalanace = singletonInt.incl(1).incl(3)

tBalanace contains 1
tBalanace contains 2
tBalanace contains 3

sealed trait NoteName
case object A extends NoteName
case object B extends NoteName
case object C extends NoteName
case object D extends NoteName
case object E extends NoteName
case object F extends NoteName
case object G extends NoteName

sealed trait Duration
case object Whole extends Duration
case object Half extends Duration
case object Quarter extends Duration

def fractionOfWhole(duration: Duration): Double = duration match {
  case Whole => 1.0
  case Half => 0.5
  case Quarter => 0.25

}

sealed trait Symbol
case class Note(name: NoteName, duration: Duration, octave: Int) extends Symbol
case class Rest(duration: Duration) extends Symbol

def symbolDurationFraction(symbol: Symbol): Double = symbol match {
  case Note(_, duration, _) => fractionOfWhole(duration)
  case Rest(duration) => fractionOfWhole(duration)
}

val c3 = Note(C, Quarter, 3)
val r2 = Rest(Half)

symbolDurationFraction(c3)
symbolDurationFraction(r2)












