package com.walcron.chapter1

import java.io._

object Main {
  val STOP = "-1"

  def main(args: Array[String]) {
   readAndPrintWords 
   readAndMapWordsRepetition
   getMarkovChain
  }
 
  def readAndPrintWords() {
   import java.util._
   val msg = s"Read Word, type $STOP to stop"
   println(msg)
   val scanner = new Scanner(System.in)
   Stream.continually(scanner.next()).takeWhile(input => (!(input equals STOP))).foreach(println)
   println("---Completed Word reading from input---")
  }

  def readAndMapWordsRepetition() {
    import java.util._
    val msg = s"Read Sentence, type $STOP to stop"
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
}
