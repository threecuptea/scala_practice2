package org.freemind.practice

/**
  * This originated from "Functional Programming Principles in Scala" course week5 (forcomp) assignment. No assignment solution was
  * published.  I submitted all 6 assignments (No easy assignment) and got full scores.
  *
  * It's difficult task.  However, wa ell-thought functional programming can provide graceful not-so-complicated solution.
  *
  * Word anagrams means words with the same combination of characters.  Ex, "eat", "ate", "tea" are words anagrams of
  * ('a', 1), ('e', 1), ('t', 1) which we call it "Occurrences" in this assignment.    "married", "admirer" are ones etc.
  *
  * Sentence anagrams are even more complicated. "Yes", "man" use ('a', 1), ('e', 1), ('m', 1) ('n', 1), ('s', 1), ('y', 1).
  * We can use ('e', 1)('n', 1) first then ('a', 1)('s', 1), then ('m', 1)('y', 1), you will get 'en', 'as', 'my'.
  * You can use ('y', 1)('e', 1)('s', 1) then ('m', 1)('a', 1)('n', 1), you get will 'Yes', "man".  As long a sentence,
  * use exactly those "Occurrences", no more and no less.
  *
  * We draw all words from dictionary which sources from forcomp/ linuxwords.txt.  Sentence anagrams do not need to be meanful
  * sentence.
  *
  * The implementation follow a nature order:
  *
  * 1. wordOccurrences and sentenceOccurrences: find "Occurrences", the combination of Char and count.  Ignore spaces
  * between words in a sentence.
  *
  * 2. dictionaryByOccurrences: create a map with the "Occurrences" as the key and List of words using it as the value.
  *
  * 3. wordAnagrams: process a word into "Occurrences" and use dictionaryByOccurrences to a List of word associated
  * with that occurrences
  *
  * 4. combinations is util function used by sentenceAnagrams.  In order to get sentenceAnagrams for ex. "Yes", "man",
  * I have to know what I can de-compose ('a', 1), ('e', 1), ('m', 1) ('n', 1), ('s', 1), ('y',1) into and their
  * combination.  We implement it using for-comprehension
  *
  * 5. subtract is another util function used by sentenceAnagrams.  That say, I use out ('e', 1)('n', 1) and find "en"
  * wordAnagrams for "Yes", "man".  I have to find what is left is ('a', 1), ('m', 1), ('s', 1), ('y', 1)
  * and continue find wordAnagrams for its combinations etc.  I added "add" for fun.
  *
  * 6. sentenceAnagrams: our goal
  *
  * @author sling/ threecuptea, 2019-01
  *
  */
object Anagrams {

  type Word = String

  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    *  how often the character appears.
    *  This list is sorted alphabetically w.r.t. to the character in each pair.
    *  All characters in the occurrence list are lowercase.
    */
  type Occurrences = List[(Char, Int)]

  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase.toList.groupBy(ch => ch).mapValues(_.length).toList.sortBy(_._1)

  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    *  the words that have that occurrence count.
    *  This lazy map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    *  For example, the word "eat" has the following character occurrence list:    *
    *     `List(('a', 1), ('e', 1), ('t', 1))`
    *
    *  This means that the `dictionaryByOccurrences` map will contain an entry:    *
    *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val map = dicts.toList groupBy (wordOccurrences) withDefaultValue List[Word]()
    source.get.close()
    map
  }
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurrences = wordOccurrences(word)
    dictionaryByOccurrences(occurrences)
  }

  /** Returns the list of all subsets of the occurrence list.
    *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are (order does not matter):
    *  *
    *    List(
    *      List(),
    *      List(('a', 1)),
    *      List(('a', 2)),
    *      List(('b', 1)),
    *      List(('a', 1), ('b', 1)),
    *      List(('a', 2), ('b', 1)),
    *      List(('b', 2)),
    *      List(('a', 1), ('b', 2)),
    *      List(('a', 2), ('b', 2))
    *    )
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) List(List[(Char, Int)]()) //gotcha: one element instead of List[Occurrences]() which is Nil
    else {
      val (char, count) = occurrences.head
      (for {
        i <- (0 to count)
        rest <- combinations(occurrences.tail)
      } yield (char, i)::rest).toList.map(_.filter{case(ch, cnt) => cnt > 0})
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    *  The precondition is that the occurrence list `y` is a subset of
    *  the occurrence list `x` -- any character appearing in `y` must
    *  appear in `x`, and its frequency in `y` must be smaller or equal
    *  than its frequency in `x`.
    *
    *  Note: the resulting value is an occurrence - meaning it is sorted
    *  and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    if (y.isEmpty) x
    else
      y.foldLeft(x.toMap) { case (map, (char, cnt)) => map.updated(char, map(char) - cnt) }.toList.filter(_._2 > 0).sortBy(_._1)
  }

  def add(xs: Occurrences, ys: Occurrences): Occurrences = (xs, ys) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case ((x,cx) ::xsl, (y,cy)::ysl) =>
      //char comparison, kind of like mergeSort
      if (x < y) {
        (x,cx)::add(xsl, ys)
      }
      else if (y < x) {
        (y,cy)::add(xs, ysl)
      }
      else {
        // x== y
        (x, cx+cy)::add(xsl, ysl)
      }
  }

  def add2(xs: Occurrences, ys: Occurrences): Occurrences = (xs, ys) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case (x ::xsl, y::ysl) =>
      //common set of chars
      val s = (xs.map(_._1) ++ ys.map(_._1)).toSet
      val xMap = xs.toMap
      val yMap = ys.toMap
      s.foldRight(List[(Char, Int)]())((ch, z) => (ch, xMap.getOrElse(ch, 0) + yMap.getOrElse(ch, 0))::z ).sortBy(_._1)
  }




  /** Returns a list of all anagram sentences of the given sentence (order does not matter
    *
    *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    *    List(
    *      List(en, as, my),
    *      List(en, my, as),
    *      List(man, yes),
    *      List(men, say),
    *      List(as, en, my),
    *      List(as, my, en),
    *      List(sane, my),
    *      List(Sean, my),
    *      List(my, en, as),
    *      List(my, as, en),
    *      List(my, sane),
    *      List(my, Sean),
    *      List(say, men),
    *      List(yes, man)
    *    )
    *  Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def loop(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(List[Word]()) //gotcha: instead of  List[Sentence]() which is Nil
      else
        for {
          occur <- combinations(occurrences)
          next <- dictionaryByOccurrences(occur)
          rest <- loop(subtract(occurrences, occur))
        } yield next::rest

    }

    loop(sentenceOccurrences(sentence))
  }


}





