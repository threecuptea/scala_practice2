package org.freemind.practice


/**
  * Telecoder is an example used in a session taught in "Functional Programming Principles in Scala’ course.
  * You can regard this as a preview of Anagrams assignment.  However, it won't go so far as Anagrams
  *
  *  1. charCodes: create a reverse map from alpha Char to number code
  *  2. wordCode: get number codes of a word
  *  3. wordsForNum: process dictionary and create a map with numeric tele-code as the key and all words associated with
  *     it as the value.
  *  4. encoder and translate take a tele-code as the input output then output words in sequence use that tele-code.
  *     For example, "7225247386". "7225" can translate into "rack", "sack", "pack". If you use the next 3 "247", you
  *     might get "air" and so on. If you use "24" only, you might get "ah".   You can also "72252" instead "7225", you
  *     will get "Scala".  In Anagrams,  In an "Occurrences":('a', 1), ('e', 1), ('m', 1) ('n', 1), ('s', 1), ('y', 1),
  *     you can start with any combination.  Here, you have to follow numeric tele-code in sequence.   It is simply "take n" and
  *     "drop n" operation.
  *
  * I practiced this on 2019-01
  */
object Telecoder {

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCodes = for ((digit, str) <- mnem; ch <- str) yield (ch, digit)

  val charCodes2 = mnem.flatMap{ case (digit, str) => str.map(ch => (ch, digit)) }


  def wordCode(word: String) =
    word.toUpperCase map charCodes

  lazy val wordsForNum: Map[String, Seq[String]] = {
    val map = (dicts.toList.groupBy(wordCode) withDefaultValue Seq[String]())
    source.get.close()
    map
  }

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List[String]())
    else (for {
      n <- (1 to number.length)
      next <-  wordsForNum(number take n)
      rest <- encode(number drop n)
    } yield next::rest).toSet

  }
  // "7225247386": Ex "7225" maps something.  then continue to map the rest "247386"

  def translate(number: String): Set[String] =
    encode(number).map(ls => ls.mkString(" "))


}
