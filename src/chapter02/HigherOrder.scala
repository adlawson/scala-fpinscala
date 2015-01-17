package com.adlawson.fpinscala.chapter02

object MyModule {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, now: Int): Int = {
      if (0 == n) prev
      else go(n-1, now, prev+now)
    }
    go(n, 0, 1)
  }
}
