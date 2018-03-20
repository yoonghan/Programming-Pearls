package com.walcron.etc

import scala.collection.Searching._
import _root_.scala.collection.JavaConversions._
import java.util.{Collections, List => JList}

class ObjectArrayTools[T <: Int](a: Array[T]) {
   def binarySearch(key: T) = {
     java.util.Arrays.binarySearch(a.asInstanceOf[Array[Int]],key)
   }
}

class SearchableSeq[T](a: Seq[T])(implicit ordering: Ordering[T]) {
    val list: JList[T] = a.toList
    def binarySearch2(key: T): Int = Collections.binarySearch(list, key, ordering)
}

object BinarySearch {
  implicit def anyrefarray_tools[T <: Int](a: Array[T]) = new ObjectArrayTools(a)
  implicit def seqToSearchable[T](a: Seq[T])(implicit ordering: Ordering[T]) =
          new SearchableSeq(a)(ordering)

  def main(args:Array[String]) {
    val informationArray = Array(1,2,3,4,5,6,7,8,9,10,11,12,14,15,18,20,22,23,25,26)
    val informationList = List(1,2,3,4,5,6,7,8,9,10,11,12,14,15,18,20,22,23,25,26)
    //val sortedArray = sortList(informationArray)
    val sortedArray = informationArray
    val sortedList = informationList

    for(x <- 0 to 2) {
      val startTime = System.nanoTime
      val result = binarySearch(sortedArray, 5)
      val result2 = binarySearch(sortedArray, 19)
      println(s"Custom search time elapsed: ${(System.nanoTime - startTime)}")

      val startTime2 = System.nanoTime
      val result3 = sortedArray.search(5)
      val result4 = sortedArray.search(19)
      println(s"Scala search time elapsed: ${(System.nanoTime - startTime2)}")

      val startTime3 = System.nanoTime
      val result5 = sortedArray.binarySearch(5)
      val result6 = sortedArray.binarySearch(19)
      println(s"Java search casting time elapsed: ${(System.nanoTime - startTime3)}")

      val startTime4 = System.nanoTime
      val result7 = sortedList.binarySearch2(5)
      val result8 = sortedList.binarySearch2(19)
      println(s"Java search as list time elapsed: ${(System.nanoTime - startTime4)}")


      val startTime9 = System.nanoTime
      val result10 = binarySearchWithImplicitConversion(sortedArray, 5)
      val result11 = binarySearchWithImplicitConversion(sortedArray, 19)
      println(s"Custom generic time elapsed: ${(System.nanoTime - startTime9)}")

      println("---")
    }
  }

  /*def sortList(sortedArray:Array[Int]):Array[Int] = {
    import com.walcron.etc.Quicksort._
    sort(sortedArray)
  }*/


  def binarySearchIntOnly(sortedArray:Array[Int], valueToBeSearch:Int):Int =  {
    def search(start:Int, end:Int):Int = {
      val pos = ((end - start) / 2) + start
      val curr = sortedArray(pos)

      if(curr == valueToBeSearch) {
        pos
      }
      else if((end - start) <= 1) {
        -1 * (pos + 1) // Indicates the value should be inserted
      }
      else if(valueToBeSearch > curr) {
        search(pos, end)
      }
      else {
        search(start, pos)
      }
    }

    search(0, sortedArray.length)
  }

  //def binarySearch[T <% Ordered[T]](sortedArray:Array[T], valueToBeSearch:T)(implicit t:ClassTag[T]):Int =  {
  def binarySearchWithImplicitConversion[T <% Ordered[T]](sortedArray:Array[T], valueToBeSearch:T):Int =  {
    def search(start:Int, end:Int):Int = {
      val pos = ((end - start) / 2) + start
      val curr = sortedArray(pos)

      if(curr == valueToBeSearch) {
        pos
      }
      else if((end - start) <= 1) {
        -1 * (pos + 1) // Indicates the value should be inserted
      }
      else if(valueToBeSearch > curr) {
        search(pos, end)
      }
      else {
        search(start, pos)
      }
    }

    search(0, sortedArray.length)
  }

  def binarySearch[T: Ordering](sortedArray:Array[T], valueToBeSearch:T):Int =  {
    import Ordering.Implicits._
    def search(start:Int, end:Int):Int = {
      val pos = ((end - start) / 2) + start
      val curr = sortedArray(pos)

      if(curr == valueToBeSearch) {
        pos
      }
      else if((end - start) <= 1) {
        -1 * (pos + 1) // Indicates the value should be inserted
      }
      else if(valueToBeSearch > curr) {
        search(pos, end)
      }
      else {
        search(start, pos)
      }
    }

    search(0, sortedArray.length)
  }
}
