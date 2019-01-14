package org.freemind

import java.io.FileNotFoundException
import scala.io.Source
import scala.util.Try

/**
  * This is a common util package object to retrieve dictionary.  dicts is a iterator[String]. It won't be expanded until
  * toList is called in dictionaryByOccurrences of Anagrams and wordsForNum in Telcoder.  Both are declared as
  * "lazy val". It will be expanded and kept when it was called the first time.
  */
package object practice {

  val resource = "forcomp/linuxwords.txt"

  val source = Try(Source.fromResource(resource)).recover{
    //Only handle Exception
    case e: NullPointerException => throw new FileNotFoundException(resource)
    case _ => throw new IllegalArgumentException(s"Unexpected exception when load $resource")
  }

  val dicts = source.get.getLines().filter(w => w.forall(_.isLetter))

  //not closing yet, the client application using this will close it.  Telcoder and Anagrams both use it.
}
