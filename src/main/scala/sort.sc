
  def insertionSortInt(xs: List[Int]): List[Int] = {
    def insert(y: Int, ys: List[Int]): List[Int] = {
      ys match {
        case List() => y :: List()
        case z :: zs =>
          if (y < z)
            y :: z :: zs
          else
            z :: insert(y, zs)

      }
    }

    xs match {
      case List() => List()
      case y :: ys =>  insert(y, insertionSortInt(ys))

    }
  }

  val shortNums = List(3, 2, 1, 8, 4, 6, 7, 5)
  insertionSortInt(shortNums)

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

  val nums = List(13, 17, 2, 9, 19, 5, 20, 3, 16, 8, 15, 4, 18, 7, 12, 1, 14, 6, 11, 10)
  val fruits = List("apple", "pear", "orange", "pineapple", "kiwi", "banana", "melon", "strawberry", "honeydew")

  insertionSort(nums)
  insertionSort(fruits)


  def mergeSortInt(xs: List[Int]): List[Int] = {
    def merge(ys: List[Int], zs: List[Int]): List[Int] = {
      (ys, zs) match {
        case (Nil, zs) => zs
        case (ys, Nil) => ys
        case (y::ysl, z::zsl) =>
          if (y < z)
            y::merge(ysl, zs)
          else
            z::merge(ys, zsl)
      }
    }

    xs match {
      case List() => List()
      case x::Nil => xs
      case _ =>
        val m = xs.length / 2
        val (ys, zs) = xs.splitAt(m)
        merge(mergeSortInt(ys), mergeSortInt(zs))
    }
































  }

  mergeSortInt(shortNums)

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(ys: List[T], zs: List[T]): List[T] = {
      (ys, zs) match {
        case (Nil, zs) => zs
        case (ys, Nil) => ys
        case (y::ysl, z::zsl) =>
          if (ord.lt(y, z))
            y::merge(ysl, zs)
          else
            z::merge(ys, zsl)
      }
    }

    xs match {
      case List() => List()
      case x::Nil => xs
      case _ =>
        val m = xs.length / 2
        val (ys, zs) = xs.splitAt(m)
        merge(mergeSort(ys), mergeSort(zs))
    }
  }

  mergeSort(nums)
  mergeSort(fruits)




















