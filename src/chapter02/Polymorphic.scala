package com.adlawson.fpinscala.chapter02

object Polymorphic {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int, prev: A): Boolean =
      if (as.length == n) true
      else if (ordered(as(n), prev)) go(n+1, as(n))
      else false
    if (as.length == 0) true
    else go(1, as(0))
  }
}
