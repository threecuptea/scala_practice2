package org.freemind.practice

import org.scalatest._
import Anagrams._

class AnagramsFunSpec extends FunSpec {

  describe("#wordOccurrences and #sentenceOccurrences") {
    val word1 = "Occurrences"
    val word2 = "Combination"
    val sentence = List(word1, word2)
    val answerWord1 = List(('c',3), ('e',2), ('n',1), ('o',1), ('r',2), ('s',1), ('u',1))
    val answerWord2 = List(('a',1), ('b',1), ('c',1), ('i',2), ('m',1), ('n',2), ('o',2), ('t',1))
    val answerSentence = List(('a',1), ('b',1), ('c',4), ('e',2), ('i',2), ('m',1), ('n',3), ('o',3), ('r',2), ('s',1), ('t',1), ('u',1))

    it(s"should return wordOccurrence of '$word1'") {
      assert(wordOccurrences(word1) === answerWord1)
    }
    it(s"should return wordOccurrence of '$word2'") {
      assert(wordOccurrences(word2) === answerWord2)
    }
    it(s"should return sentenceOccurrence of '$sentence'") {
      assert(sentenceOccurrences(sentence) === answerSentence)
    }

  }

  describe("#add and #substract") {
    val a = List(('a',1), ('m',1), ('n',1))
    val b = List(('e',1), ('s',1), ('y',1))
    val c = List(('a',1), ('e',1), ('m',1), ('n',1), ('s',1), ('y',1))

    it(s"c substract a should be b") {
      assert(subtract(c, a) === b)
    }
    it(s"c substract b should be a") {
      assert(subtract(c, b) === a)
    }
    it(s"c substract c should be empty list") {
      assert(subtract(c, c) === List())
    }
    it(s" a add b should be c") {
      assert(add(a, b) === c)
    }
    it(s" a add2 b should be c") {
      assert(add2(a, b) === c)
    }
  }

  describe("#dictionaryByOccurrences") {
    val occurForEat = List(('a', 1), ('e', 1), ('t', 1))

    it ("should return 'eat', 'ate', 'tea'") {
      assert(dictionaryByOccurrences(occurForEat).toSet === Set("eat", "ate", "tea"))
    }
  }

  describe("#wordAnagrams") {

    it ("#word anagrams: married") {
      assert(wordAnagrams("married").toSet === Set("married", "admirer"))
    }
    it ("word anagrams: player") {
      assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
    }
  }

  describe("#combinations") {
    val abba = List(('a', 2), ('b', 2))
    val list2 = List(('a', 2), ('b', 3), ('c', 1))
    val list3 = List(('a',1), ('e',1), ('m',1), ('n',1), ('s',1), ('y',1))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )

    it ("combinations of list1") {
      assert( combinations(abba).toSet === abbacomb.toSet)
    }
    it ("cominations of list2") {
      assert(combinations(list2).size === 24)
    }
    it ("cominations of list3") {
      assert(combinations(list3).size === 64)
    }
  }

  describe("#sentenceAnagrams") {
    val sentence1 = List("Yes", "man")
    val sentence2 = List("Linux", "rulez")

    val answer1 = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )

    val answer2 = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )

    it ("sentenceAnagrams of sentence1") {
      assert( sentenceAnagrams(sentence1).toSet === answer1.toSet )
    }
    it ("sentenceAnagrams of sentence2") {
      assert( sentenceAnagrams(sentence2).toSet === answer2.toSet )
    }

  }




}
