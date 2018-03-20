package com.walcron.etc

import scala.reflect.ClassTag

object Quicksort {
  def main(args:Array[String]) {
    if(args.length==0) {
      println(sort(Array("8","7","6","5","4","3","2","1")).mkString(", "))
    }
    else {
      println(sort(args).mkString(", "))
    }
  }

  //def quickSort[T <% Ordering[T]](list:Array[T])(implicit t:ClassTag[T]):Array[T] = {
  def sort[T:Ordering](list:Array[T])(implicit t:ClassTag[T]):Array[T] = {
    import Ordering.Implicits._
    if(list.length <= 1) {
      list
    }
    else {
      val pivot = list(list.length-1)
      Array.concat(
        sort(list.filter(t => t < pivot)),
        list.filter(pivot.== ),
        sort(list.filter(t => t > pivot))
      )
    }
  }

  def sortLetter(str:String):String =  {
    val ary = str.toCharArray
    sort(ary).mkString
  }
}
