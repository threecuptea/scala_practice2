val nums = List(1,2,3,4,5)

def myFoldLeft[T, U](xs: List[T])(z: U)(op: (U, T) => U): U = xs match {
  case Nil => z
  case x::xsl => myFoldLeft(xsl)(op(z, x))(op)
}

def myFoldRight[T, U](xs: List[T])(z: U)(op: (T, U) => U): U = xs match {
  case Nil => z
  case x::xsl => op(x, myFoldRight(xsl)(z)(op))
}

myFoldLeft(nums)(0)((z, x) => z + x)

nums.reduceLeft((z, x) => z + x)
nums.foldLeft(0)((z, x) => z + x)
nums.reduceLeft((z, x) => z * x)
nums.foldLeft(1)((z, x) => z * x)


nums.reduceRight((x, z) => x + z)
nums.foldRight(0)((x, z) => x + z)

myFoldRight(nums)(1)((x, z) => x * z)


val l12 = List(1,2)                             //> l12  : List[Int] = List(1, 2)
val l34 = List(3,4)                             //> l34  : List[Int] = List(3, 4)
val l56 = List(5,6)                             //> l56  : List[Int] = List(5, 6)
val l78 = List(7,8)

val l1234 = List(1,2,3,4)                       //> l1234  : List[Int] = List(1, 2, 3, 4)
val l5678 = List(5,6,7,8)
//> l5678  : List[Int] = List(5, 6, 7, 8)
val l12345678 = List(1,2,3,4,5,6,7,8)

//O(n)
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case x::xsl => x::concat(xsl, ys)
}
//O(n)
def concatRight[T](xs: List[T], ys: List[T]): List[T] =
  xs.foldRight(ys)((x, z) => x::z)

concat(l1234, l5678)
concatRight(l1234, l5678)

def flatten(ls: List[Any]): List[Any] = ls flatMap {
  case l: List[_] => flatten(l)
  case e => List(e)
}

val cList = List(List(1, List(2, 3)), List(List(List(List(4)), List(5)), List(6, 7)), 8, Nil)
flatten(cList)

def myFlatten[T](xs: List[List[T]]): List[T] = xs match {
  case Nil => Nil
  case x::xsl => x ++ myFlatten(xsl)
}

myFlatten(List(l12, l34, l56, l78))

def myFlattenRight[T](xs: List[List[T]]): List[T] =
  xs.foldRight(List[T]())((x, z) => x ++ z)
//::: operation proportional to the first list, the first list stay at 1 element, O(n) operation
myFlattenRight(List(l12, l34, l56, l78))

//take(n), drop(n), splitAt(n), filter(), filterNot(), partition, takeWhile, dropWhile, span
def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case x::xsl => reverse(xsl) ++ List(x)
  //::: operation proportional to the first list, (n-1)+(n-2) .....+1 n*(n-1) / 2
}

def reverseLeft[T](xs: List[T]): List[T] =
  xs.foldLeft(List[T]())((z, x) => x::z)
//:: happen to work perfectly in this case, first element would cons first, stay at the last, :: is o(n)
reverse(l12345678)
reverseLeft(l12345678)
