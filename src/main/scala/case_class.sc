
case class Note(name: String, duration: String, octave: Int)

val c3 = Note("C", "Quarter", 3)
val cThree = Note("C", "Quarter", 3)

c3.toString
c3.name
c3 == cThree

val c4 = c3.copy(octave = 4)
c4.toString

def matchNote(obj: AnyRef) = obj match {
  case Note(name,  duration, octave) => s"The duration of Note is $duration"
  case _ => s"It's not a note"
}

matchNote(cThree)


class MyNote(_name: String, _duration: String, _octave: Int) {
  val name = _name
  val duration = _duration
  val octave = _octave

  override def toString() = s"MyNote($name,$duration,$octave)"

  override def equals(other: Any): Boolean = other match {
    case that: MyNote =>
      (this canEqual that) &&
      name == that.name
      duration == that.duration
      octave == that.octave
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MyNote]

  override def hashCode(): Int = {
    val state = Seq(name, duration, octave)
    state.map(_.hashCode).foldLeft(0)((a, b) => a * 31 + b)
  }

  def copy(name: String = name, duration: String = duration, octave: Int = octave) =
    new MyNote(name, duration, octave)

}

object MyNote {
  def apply(name: String, duration: String, octave: Int) =
    new MyNote(name, duration, octave)

  //You normally use ==, it routes to equals, except that it treats nulls properly.
  // Reference equality (rarely used) is eq.
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

val m4 = m3.copy(octave = 4)
m4.toString

def matchMyNote(obj: AnyRef) = obj match {
  case MyNote(name,  duration, octave) => s"The duration of Note is $duration"
  case _ => s"It's not a note"
}

matchMyNote(mThree)


abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < elem)
      new NonEmpty(elem, left.incl(x), right)
    else if (x > elem)
      new NonEmpty(elem, left, right.incl(x))
    else
      this
  }

  def contains(x: Int): Boolean = {
    if (x < elem)
      left.contains(x)
    else if (x > elem)
      right.contains(x)
    else
      true
  }
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int) = false

}

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

val nums = List(1, 2, 3, 4, 5)

Product.reduce(nums)
Sum.reduce(nums)