
class Rational(x: Int, y: Int) extends Ordered[Rational] {
  //alternative constructor
  def this(x: Int) = this(x, 1)
  require(y > 0, "denominator cannot be 0")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  lazy val numer: Int = x / g
  lazy val denom: Int = y / g

  def + (that: Rational) =
    new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

  def - (that: Rational) =
    new Rational(this.numer * that.denom - that.numer * this.denom, this.denom * that.denom)

  def * (that: Rational) =
    new Rational(this.numer * that.numer, this.denom * that.denom)

  def / (that: Rational) =
    new Rational(this.numer * that.denom, this.denom * that.numer)

  override def compare(that: Rational) = this.numer * that.denom - that.numer * this.denom

  override def toString = numer + "/" + denom

}

val x = new Rational(1, 2)
val y = new Rational(1, 3)
x * x + y * y

def insertionSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  def insert(y: T, ys: List[T]): List[T] = {
    ys match {
      case List() => y :: List()
      case z :: zs =>
        if (ord.lt(y, z))
          y :: z :: zs
        else
          z :: insert(y, zs)

    }
  }

  xs match {
    case List() => List()
    case y :: ys =>  insert(y, insertionSort(ys))

  }
}


//implicit val rationalOrder: Ordering[Rational] = new Ordering[Rational] {
//  override def compare(x: Rational, y: Rational) = x.numer * y.denom - y.numer * x.denom
//}

val half = new Rational(1, 2)
val third = new Rational(1, 3)
val fourth = new Rational(1, 4)
val rationals = List(third, half, fourth)
insertionSort(rationals)

