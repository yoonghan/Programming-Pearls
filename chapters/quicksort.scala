package com.walcron.chapter1

object Quicksort {
  def main(args:Array[String]) {
    if(args.length==0) {
      println(quickSort(Array("8","7","6","5","4","3","2","1")).mkString(", "))
    }
    else {
      println(quickSort(args).mkString(", "))
    }
  }

  def quickSort(list:Array[String]):Array[String] = {
    if(list.length <= 1) {
      list
    }
    else {
      val pivot = list(list.length-1)
      Array.concat(
        quickSort(list.filter(pivot.>)),
        list.filter(pivot.== ),
        quickSort(list.filter(pivot.<))
      )
    }
  }
}
