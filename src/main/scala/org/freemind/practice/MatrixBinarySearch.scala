package org.freemind.practice

/**
  * Write a binary search which is in teh context of matric (multi-dimension array)
  *
  * @author sling/threecuptea 2019-01-11
  */
object MatrixBinarySearch {

  //In ascending order.
  val arr: Array[Array[Double]] = Array(
    Array(1.0, 5.6, 7.8),
    Array(12.7, 15.4, 18.3),
    Array(20.7, 23.4, 43.7)
  )

  val row = arr.length
  val col = arr(0).length
  val n = row * col

  var target: Double = 0.0

  def loop(start: Int, end: Int): Boolean = {
    if (start > end) {
      false
    } else {
      val mid = (start + end) / 2
      val rowIdx = mid / col
      val colIdx = mid % col
      val curr: Double = arr(rowIdx)(colIdx)
      if (curr == target) {
        true
      } else if (target > curr) {
        loop(mid + 1, end)
      }
      else {
        loop(start, mid -1)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    target = 18.3
    println(s"${target} found = ${loop(0, n -1)}")

    target = 4.4
    println(s"${target} found = ${loop(0, n -1)}")

    target = 43.7
    println(s"${target} found = ${loop(0, n -1)}")

    target = 100.0
    println(s"${target} found = ${loop(0, n -1)}")

    target = 0.0
    println(s"${target} found = ${loop(0, n -1)}")
  }

}
