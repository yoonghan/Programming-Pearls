package com.walcron.etc

import scala.reflect.ClassTag

object Quicksort {
  def main(args:Array[String]) {
    if(args.length==0) {
      println(quickSort(Array("8","7","6","5","4","3","2","1")).mkString(", "))
    }
    else {
      println(quickSort(args).mkString(", "))
    }
  }

  def quickSort[T <% Ordered[T]](list:Array[T])(implicit t:ClassTag[T]):Array[T] = {
    if(list.length <= 1) {
      list
    }
    else {
      val pivot = list(list.length-1)
      Array.concat(
        quickSort(list.filter(t => t < pivot)),
        list.filter(pivot.== ),
        quickSort(list.filter(t => t > pivot))
      )
    }
  }

  def quickSortLetter(str:String):String =  {
    val ary = str.toCharArray
    quickSort(ary).mkString
  }
}
