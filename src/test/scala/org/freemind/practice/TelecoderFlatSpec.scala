package org.freemind.practice

import org.scalatest.{BeforeAndAfter, FlatSpec}
import Telecoder._

class TelecoderFlatSpec extends FlatSpec with BeforeAndAfter {

  val word = "Java"
  val charcodes = "7225247386"
  val charcode7225 = "7225"
  val charcode72252 = "72252"
  val encode7225 = Set(List("rack"), List("sack"), List("pack"))
  val encode72252 = Set(List("Scala"))
  val encodeSet = Set(
    List("rack", "ah", "re", "to"), List("sack", "ah", "re", "to"), List("Scala", "ire", "to"), List("sack", "air", "fun"),
    List("rack", "air", "fun"), List("rack", "bird", "to"), List("pack", "air", "fun"), List("pack", "ah", "re", "to"),
    List("pack", "bird", "to"), List("Scala", "is", "fun"), List("sack", "bird", "to")
  )
  val translateSet = Set(
    "sack bird to", "sack air fun", "rack air fun", "rack ah re to", "pack air fun", "rack bird to",
    "Scala is fun", "sack ah re to", "pack bird to", "Scala ire to", "pack ah re to")


  "charCodes" should "map Java to 5282" in {
    assert( word.toUpperCase.map(charCodes) === "5282" )
  }

  "charCodes2" should "map Java to 5282" in {
    assert( word.toUpperCase.map(charCodes2) === "5282" )
  }

  "wordCode" should "map Java to 5282" in {
    assert( wordCode(word) === "5282" )
  }

  "7225" should "encode should be the followings:" in {
    assert( encode(charcode7225) === encode7225 )
  }

  "72252" should "encode should be the followings:" in {
    assert( encode(charcode72252) === encode72252 )
  }

  "7225247386" should "encode should be the followings:" in {
    assert( encode(charcodes) === encodeSet )
  }

  "7225247386" should "translate should be the followings:" in {
    assert( translate(charcodes) === translateSet )
  }


}
