

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

identityMatrix(5)

def rotate[T](xs: List[T]): List[List[T]] = {
  def loop(ys: List[T], n: Int): List[List[T]] =
    if (n == 0) Nil
    else
      ys :: loop(ys.tail ++ List(ys.head), n-1)
  loop(xs, xs.length)
}

val nums = List(1,2,3,4,5)
rotate(nums)
rotate(List("Mon.", "Tues.", "Wed.", "Thur."))



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

def groupByIndex[T](xs: List[(T, Int)]): List[List[(T, Int)]] = xs match {
  case Nil => Nil
  case x::xsl =>
    val (fst, snd) = xs.partition(_._2 == x._2)
    fst::groupByIndex(snd)
}

def transpose[T](matrix: List[List[T]]): List[List[T]] =
  groupByIndex(matrix.flatMap(l => l.zipWithIndex)).map(l => l.map(_._1))

transpose(matrix1)
transpose(matrix2)
transpose(matrix3)

val chars = List('a','b','c','d')

def combinations2[T](xs: List[T]): List[List[T]] = {
  val xsWithIndex = xs.zipWithIndex
  xsWithIndex.flatMap{case (x, i) => xsWithIndex.filter{case (y, j) => j > i}.map{ case (y, j) => List(x, y)}}
}

def combinations22[T](xs: List[T]): List[List[T]] = {
  val idx = 0 until xs.size
  (for {
    i <- idx
    j <- idx if j > i
  } yield xs(i)::xs(j)::Nil).toList
}

combinations2(chars)
combinations22(chars)

def combinations3[T](xs: List[T]): List[List[T]] = {
  val xsWithIndex = xs.zipWithIndex
  xsWithIndex.flatMap{case (x, i) => xsWithIndex.filter{case (y, j) => j > i}.
    flatMap{case (y, j) => xsWithIndex.filter{case(z, k) => k > j}.map{case(z, k) => List(x, y, z)}}}
}

def combinations32[T](xs: List[T]): List[List[T]] = {
  val idx = 0 until xs.size
  (for {
    i <- idx
    j <- idx if j > i
    k <- idx if k > j
  } yield xs(i)::xs(j)::xs(k)::Nil).toList
}

combinations3(chars)
combinations32(chars)

def combinations[T](xs: List[T], n: Int): List[List[T]] = {
  if (n == 0 || xs.length == 0) {
    List(List[T]())
  }
  else {
    (for {
      i <- (0 until xs.length)
      rest <- combinations(xs.drop(i+1), n-1)
    } yield xs(i)::rest).filterNot(ls => ls.length < n).toList
  }
}

chars.combinations(2).toList
combinations(chars, 2)
chars.combinations(3).toList
combinations(chars, 3)
nums.combinations(3).toList
combinations(nums, 3)



