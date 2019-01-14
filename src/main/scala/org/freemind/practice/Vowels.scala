package org.freemind.practice

/**
  * Swap only vowels in a string. Ex. "hello" =>  "holle", "academic" => "icedamac", "aei" => "iea" etc.
  *  I have three implementations: Vowels, Vowels2, VowelIterate.
  *
  * This is foldRight solution and the most compact and clean version.  I don't need to reverse lookup or the end result.
  * Thanks to foldRight, it process one at a time to the end result in reverse order.
  * when the processed char is a consonant, just pre-pend it, otherwise consume and prepend one character from the lookup to the result.
  *
  * The lookup is simply a filtered vowel list in original order.  Since we process original list in the reverse order,
  * I essentially put vowels in reverse order. It's good match.  That make this implementation a graceful functional solution.
  *
  */
object Vowels {

  val VOWELS = Set('a', 'e', 'i', 'o', 'u')
  def isVowel(c: Char): Boolean = VOWELS.contains(c)

  def swapVowels(str: String): String = {
    //(accum-result, vows_in_str), foldRight, characters traverse in reverse order.  if isVowel, pick up value from lookup else pick up c
    str.foldRight((List[Char](), str.filter(isVowel))) { case (c, (rs, lookup)) =>
        if (isVowel(c))
          (lookup.head::rs, lookup.tail)
        else
          (c::rs, lookup)
    }._1.mkString
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
