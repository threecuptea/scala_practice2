

def group[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x::xsl =>
    val (fst, snd) = xs.partition(_ == x)
    fst::group(snd)
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x::xsl =>
    val (fst, snd) = xs.span(_ == x)
    fst::pack(snd)
}

val aList =List("a","a","a","b","c","c","a","b","b")

group(aList)
pack(aList)

def countBy[T](xs: List[T])(f: List[T] =>  List[List[T]]): List[(T, Int)] =
  f(xs).map(ls => (ls.head, ls.length))

countBy(aList)(group)
countBy(aList)(pack)

aList.groupBy(c => c).map{case (k, ls) => (k, ls.length)}.toList.sortBy(_._1)

def flatten(ls: List[Any]): List[Any] = ls flatMap  {
  case l: List[_] => flatten(l)
  case e => List(e)
}

val cList = List(List(1, List(2, 3)), List(List(List(List(4)), List(5)), List(6, 7)), 8, Nil)

flatten(cList)

/*
    1000000
    0100000
    0010000
    0001000
    0000100
    0000010
    0000001
  */
def identityMatrix(n: Int): Seq[Seq[Int]] = {
  (0 until n).map(i => List.fill(n)(0).updated(i, 1))
}

identityMatrix(4)

//Vector(Vector(1, 2, 3, 4, 5), Vector(2, 3, 4, 5, 1),
//|  Vector(3, 4, 5, 1, 2), Vector(4, 5, 1, 2, 3), Vector(5, 1, 2, 3, 4))
//base (1,2,3,4,5) The first list provide + num
def rotation(n: Int): Seq[Seq[Int]] = {
  (0 until n).map(i => (1 to n).map{ j =>
    val k = i + j
    if (k > n) k -n else k
  })
}

rotation(5)

def rotation2(n: Int): Seq[Seq[Int]] = {
  def helper(xs: List[Int], c: Int): List[List[Int]] = {
    if (c == 0) Nil
    else {
      xs :: helper(xs.tail ++ List(xs.head), c -1)
    }
  }
  helper((1 to n).toList, n)
}

rotation2(5)


val matrix1 = List(List(1,2),List(3,4), List(5,6))
//> matrix1  : List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6))
val matrix2 = List(List(1,2,3),List(4,5,6), List(7,8,9))
//> matrix2  : List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8,
//| 9))
val matrix3 = List(List(1,2,3,4),List(5,6,7,8)) //> matrix3  : List[List[Int]] = List(List(1, 2, 3, 4), List(5, 6, 7, 8))

matrix1.transpose                               //> res30: List[List[Int]] = List(List(1, 3, 5), List(2, 4, 6))
matrix2.transpose                               //> res31: List[List[Int]] = List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
matrix3.transpose                               //> res32: List[List[Int]] = List(List(1, 5), List(2, 6), List(3, 7), List(4, 8
//| ))
//groupBy Map(1 -> List((2,1), (4,1),(6,1)
//matrix1.flatMap(ls => ls.zipWithIndex).groupBy(_._2).toList.sortBy(_._1).map(_._2).map(ls => ls.map(_._1))

def myTranspose(xs: List[List[Int]]) =
  xs.flatMap(ls => ls.zipWithIndex).groupBy(_._2).toList.sortBy(_._1).map(_._2).map(ls => ls.map(_._1))

myTranspose(matrix1)
myTranspose(matrix2)
myTranspose(matrix3)

def groupByIndex[T](xs: List[(T, Int)]): List[List[(T, Int)]] = xs match {
  case Nil => Nil
  case x::xsl =>
    val (fst, snd) = xs.partition(_._2 == x._2)
    fst::groupByIndex(snd)
}

def myTranspose2(xs: List[List[Int]]) =
  groupByIndex(xs.flatMap(ls => ls.zipWithIndex)).map(ls => ls.map(_._1))

myTranspose2(matrix1)

List(1,2,3,4).combinations(2).toList
List(1,2,3,4).permutations.toList

val v1 = Vector(1,3,5)                          //> v1  : scala.collection.immutable.Vector[Int] = Vector(1, 3, 5)
val v2 = Vector(2,4,6)

def dotProduct(v1: Vector[Int], v2: Vector[Int]): Int =
  (v1 zip v2).foldLeft(0) {(z, x) => z + x._1 * x._2}

dotProduct(v1, v2)

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


