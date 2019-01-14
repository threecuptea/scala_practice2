package org.freemind.practice

import scala.collection.mutable.ListBuffer


/**
  * The iterative (not functional) solution for swap vowels in a string.
  *
  * This literally translate what I did in Java.  In java I use for (int i = 0; i < s.length; i++) instead of while.
  *
  * I cannot use for-comprehensions here. The for-comprehensions block is a self-contained closure.  It won't be able to
  * update external variable j.  Therefore, j will be initialized to its initial value s.length -1 in each f
  * or-comprehension loop.  I will get wrong answer "hollo" for "hello".
  * A good lesson to understand the difference between traditional for/ while loop and Scala for-comprehensions.
  */
object VowelIterate {

  val VOWELS = Set('a', 'e', 'i', 'o', 'u')
  def isVowel(c: Char): Boolean = VOWELS.contains(c)

  def swapVowels(s: String): String = {
    var j = s.length -1
    var i = 0
    val ls = ListBuffer[Char]() //otherwise reverse is involved
    while (i < s.length) {
      val c = s(i)
      if (isVowel(c)) {
        while (!isVowel(s(j)))
          j -= 1
        ls += s(j)
      }
      else {
        ls += c
      }
    }
    ls.mkString
  }

  def testSwapVow(): Unit = {
    assert(swapVowels("hello") == "holle")
    assert(swapVowels("aei") == "iea")
    assert(swapVowels("foobar") == "faobor")
    assert(swapVowels("academic") == "icedamac")
    assert(swapVowels("hello world") == "hollo werld")
  }

  def main(args: Array[String]): Unit = {
    testSwapVow()
  }

}
