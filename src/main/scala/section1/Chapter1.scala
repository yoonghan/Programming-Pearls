package com.walcron.chapter1

import java.io._

object Main {
  val STOP = "-1"

  def main(args: Array[String]) {
   readAndPrintWords
   readAndMapWordsRepetition
   getMarkovChain
   searchForWords
  }

  def readAndPrintWords() {
   import java.util._
   val msg = s"[Word] Read Word, type $STOP to stop"
   println(msg)
   val scanner = new Scanner(System.in)
   Stream.continually(scanner.next()).takeWhile(input => (!(input equals STOP))).foreach(println)
   println("---Completed Word reading from input---")
  }

  def readAndMapWordsRepetition() {
    import java.util._
    val msg = s"[Mapping] Read Sentence, type $STOP to stop"
    println(msg)
    val scanner = new Scanner(System.in)
    Stream.continually(scanner.nextLine()).takeWhile(input => (!(input equals STOP))).foreach{
      (readLine:String) =>
      bakeMeAMap(readLine);
      bakeMeAnotherMap(readLine)
    }
    println("---Completed Mapping of words to repetition---")
  }

  /**
   * Requires O n*2
   */
  def bakeMeAMap(input:String):Unit = {
    val startTime = System.nanoTime
    val bagOfWords = input.split(" ")
    val initMap:Map[String, Int] = Map()
    val result = bagOfWords.foldLeft(initMap)((map:Map[String, Int], word:String) => map ++ Map(word -> (map.getOrElse(word, 0)+1)))
    println(result)
    logTime(startTime)
  }

  /**
   * Required O n*3 time as groupBy loops again then length loops again
   */
  def bakeMeAnotherMap(input:String):Unit = {
    val startTime = System.nanoTime
    val bagOfWords = input.split(" ").toList
    val result = bagOfWords.groupBy(identity).mapValues(_.length)
    println(result)
    logTime(startTime)
  }

  def logTime(startTime:Long) = {
    println("It took:"+(System.nanoTime - startTime))
  }

  /**
   * Half baked, but this is for Machine learning, markov chain + n-gram
   */
  def getMarkovChain() {
    val naturalSentence = "I am Sam"
    val tokens = naturalSentence.split(" ")
    //val biggrams = for (i <- 0 until tokens.length-1) yield (tokens(i), tokens(i+1))
    val biggrams = (0 until tokens.length-1).map(i => (tokens(i), tokens(i+1))).toList
    println(biggrams)
    println("---Completed N-th gram of Markov Chain ---")
  }

  /**
   * Sort words, did not cater for repeated words.
   **/
  def searchForWords() {
    import java.util._
    import scala.collection.mutable.ArrayBuffer
    val msg = s"[Signed] Read Word, type $STOP to stop"
    println(msg)
    val scanner = new Scanner(System.in)
    Stream.continually(scanner.nextLine()).takeWhile(input => (!(input equals STOP))).foreach{
      (readLine:String) =>
      sortSignedWords(readLine)
    }
    println("---Completed [Signed] from input---")
  }

  def sortSignedWords(sentence:String) {
    val splittedWords = sentence.split(" ")
    val signedResult = splittedWords.map(signWords(_))
    val sortedCollection = sortCollection(signedResult)
    sortedCollection.fold("")((prev, curr) => {
      val split = curr.split(" ")
      if(split.length == 2) {
        val signed = split(0)
        val word = split(1)
        if(prev != signed) {
          println("")
        }
        print(s"${word} ")
        signed
      }
      else {
        ""
      }
    })
    println("")
  }

  def sortCollection(collection: Array[String]):Array[String] = {
    import com.walcron.etc.Quicksort._
    sort(collection)
  }

  def signWords(input:String):String = {
    import com.walcron.etc.Quicksort._
    val strBuilder = new StringBuilder()
    strBuilder.append(sort(input.toCharArray).mkString)
    strBuilder.append(" ")
    strBuilder.append(input)
    val combineSignedAndWord = strBuilder.toString
    combineSignedAndWord
  }
}
