package org.freemind.practice

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Swap only vowels in a string. Ex. "hello" =>  "holle", "academic" => "icedamac", "aei" => "iea" etc.
  * This is the second implementation
  *
  * Martin Odersky mentioned in his "Functional Programming Principles in Scala" online Coursera course,
  * for comprehension and the combination of flatMap & filter are exchangeable.  Fold solution and recursive solution
  * are sometimes exchangeable too.  However, fold has foldRight and foldLeft two options.  Efficient recursive solution resorts to
  * head and tail of Traversable trait.  It will process original list in order.  Therefore,
  * to get swap vowels to work, I have to reverse filtered vowels to be a lookup.
  *
  * I provide both simple recursive and tail recursive solutions.  Notice I use ListBuffer solution in tail recursive otherwise
  * I have to reverse the result list too.  More tail recursive solution can be found in high_order.sc.
  *
  */
object Vowels2 {

  val VOWELS = Set('a', 'e', 'i', 'o', 'u')
  def isVowel(c: Char): Boolean = VOWELS.contains(c)

 //You can use List too but have to reverse result, fold and recursive are convertable to each other. however, fold has two direction.
 //That will save the work to reverse.
 @tailrec
  def swapVowelsLoopTail(rest: String, lookup: String, result: ListBuffer[Char]): ListBuffer[Char] = {
    if (rest.isEmpty) result
    else {
      val c = rest.head
      if (isVowel(c)) {
        swapVowelsLoopTail(rest.tail, lookup.tail, result += lookup.head)
      }
      else {
        swapVowelsLoopTail(rest.tail, lookup, result += c)
      }
    }
  }

  def swapVowelsLoop(rest: String, lookup: String): List[Char] = {
    if (rest.isEmpty) Nil
    else {
      val c = rest.head
      if (isVowel(c)) {
        lookup.head::swapVowelsLoop(rest.tail, lookup.tail)
      }
      else {
        c::swapVowelsLoop(rest.tail, lookup)
      }
    }
  }


  def swapVowels(s: String): String = {
    swapVowelsLoop(s, s.filter(isVowel).reverse).mkString
  }

  def swapVowelsTail(s: String): String = {
    swapVowelsLoopTail(s, s.filter(isVowel).reverse, ListBuffer[Char]()).mkString
  }

  def testSwapVow(): Unit = {
    assert(swapVowels("hello") == "holle")
    assert(swapVowels("aei") == "iea")
    assert(swapVowels("foobar") == "faobor")
    assert(swapVowels("academic") == "icedamac")
    assert(swapVowels("hello world") == "hollo werld")
  }

  def testSwapVowTail(): Unit = {
    assert(swapVowelsTail("hello") == "holle")
    assert(swapVowelsTail("aei") == "iea")
    assert(swapVowelsTail("foobar") == "faobor")
    assert(swapVowelsTail("academic") == "icedamac")
    assert(swapVowelsTail("hello world") == "hollo werld")
  }

  def main(args: Array[String]): Unit = {
    testSwapVow()
    testSwapVowTail()
  }


}
