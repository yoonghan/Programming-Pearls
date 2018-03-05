/**
 *Using streams for fibonacci.
 **/
package com.walcron.etc

object Fibonacci {
  def fibonacci(a:Int, b:Int):Stream[Int] = a #:: fibonacci(b, a+b)

  def fibs:Stream[Int] = 0 #:: 1 #:: (fibs zip fibs.tail).map(t => t._1 + t._2)

  def main(args:Array[String]) {
    println("Fibonacci basic stream:")
    fibonacci(0,1).take(5).foreach(x => print(x +" "))
    println("")
    println("Fibonacci nonbasic stream")
    fibs.take(10).foreach(x => print(x + " "))
  }
}
