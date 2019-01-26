/*
class and case_class
 */
class MyNote(_name: String, _duration: String, _octave: Int) {

  val name = _name
  val duration = _duration
  val octave = _octave

  override def hashCode() = {
    val state = Seq(name, duration, octave)
    state.map(_.hashCode()).foldLeft(0)((a, b) => a * 31 + b)
  }

  override def equals(obj: scala.Any) = {
    obj match {
      case that: MyNote =>
        this.canEqual(that) &&
        this.name == that.name &&
        this.duration == that.duration &&
        this.octave == that.octave
      case _ => false
    }
  }

  def copy(name: String = name, duration: String = duration, octave: Int = octave): MyNote = {
    new MyNote(name, duration, octave)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MyNote]

  override def toString = s"MyNote($name,$duration,$octave)"
}

object MyNote {
  def apply(_name: String, _duration: String, _octave: Int): MyNote = new MyNote(_name, _duration, _octave)

  def unapply(note: MyNote): Option[(String, String, Int)] = {
    if (note eq null)
      None
    else
      Some((note.name, note.duration, note.octave))
  }
}

val m3 = MyNote("C", "Quarter", 3)
val mThree = MyNote("C", "Quarter", 3)

m3.toString
m3.name
m3 == mThree

val m4 = m3.copy(name="D", octave = 4)
m4.toString

def matchMyNote(obj: AnyRef): String = {
  obj match {
    case MyNote(_, duration, _) =>  s"The duration of Note is $duration"
    case _  => "This is not a note"
  }
}

matchMyNote(mThree)

//java enum
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

//abstract class is different from trait.  It can have Constructor parameter
abstract class Reducer(init: Int) {
  def combine(x: Int, y: Int): Int
  def reduce(xs: List[Int]): Int = xs match {
    case Nil => init
    case y :: ys =>
      combine(y, reduce(ys))
  }
}

object Product extends Reducer(1) {
  def combine(x: Int, y: Int) = x * y
}

object Sum extends Reducer(0) {
  def combine(x: Int, y: Int) = x + y
}

val lst = List(1, 2, 3, 4, 5)

Product.reduce(lst)
Sum.reduce(lst)


def insertionSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  //ys is sorted list
  def insert(y: T, ys: List[T]): List[T] = ys match {
    case List() => List(y)
    case z :: zs =>
      if (ord.lt(y, z)) {
        y :: ys
      }
      else {
        z :: insert(y, zs)
      }
  }
  xs match {
    case List() => List()
    case y :: ys => insert(y, insertionSort(ys))
  }
}

val nums = List(13, 17, 2, 9, 19, 5, 20, 3, 16, 8, 15, 4, 18, 7, 12, 1, 14, 6, 11, 10)
val fruits = List("apple", "pear", "orange", "pineapple", "kiwi", "banana", "melon", "strawberry", "honeydew")

insertionSort(nums)
insertionSort(fruits)

def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  def merge(ys: List[T], zs: List[T]): List[T] = (ys, zs) match {
    case (Nil, zs) => zs
    case (ys, Nil) => ys
    case (y::ysl, z::zsl) =>
      if (ord.lt(y, z)) {
        y::merge(ysl, zs)
      }
      else {
        z::merge(ys, zsl)
      }
  }
  val m = xs.length / 2
  if (m == 0) {
    xs
  }
  else {
    val (ys, zs) = xs.splitAt(m)
    merge(mergeSort(ys), mergeSort(zs))
  }
}

mergeSort(nums)
mergeSort(fruits)

class Rational(x: Int, y: Int) extends Ordered[Rational] {
  def this(x: Int) = this(x, 1)
  require(y != 0, "denominator cannot be 0")

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  override def toString = numer + "/" + denom

  def + (that: Rational) =
    new Rational(this.numer * that.denom + this.denom * that.numer, this.denom * that.denom)

  def - (that: Rational) =
    new Rational(this.numer * that.denom - this.denom * that.numer, this.denom * that.denom)

  def * (that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def / (that: Rational) =
    new Rational(this.numer * that.denom, this.denom * that.numer)

  override def compare(that: Rational) = this.numer * that.denom - this.denom * that.numer
}

val x = new Rational(1, 2)
val y = new Rational(1, 3)
x * x + y * y

val half = new Rational(1, 2)
val third = new Rational(1, 3)
val fourth = new Rational(1, 4)
val rationals = List(third, half, fourth)
insertionSort(rationals)

class FizzBuzz(mapping: List[(Int, String)]) {
  def this(bindings: (Int, String)*) = this(bindings.toList)

  def run(n: Int) = {
    (for ((d, str) <- mapping if (n % d == 0)) yield str) match {
      case Nil => n
      case l: Seq[String] => l.mkString
    }
  }
}

val fizzBuzz = new FizzBuzz(3->"Fizz",5->"Buzz",7->"Tezz")
(1 to 35).map(fizzBuzz.run)


