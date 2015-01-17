package com.adlawson.fpinscala.chapter05

import Stream._
import org.scalatest.{Matchers, FlatSpec}

class StreamTest extends FlatSpec with Matchers {
  val data = Seq(
    Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList -> List(12, 14),
    cons(11, Stream(2, 3, 4).map(_ + 20)).filter(_ % 2 == 0).toList -> List(22, 24),
    Stream(2, 3, 4).map(_ + 30).filter(_ % 2 == 0).toList -> List(32, 34),
    cons(42, Stream(3, 4).map(_ + 40)).filter(_ % 2 == 0).toList -> List(42, 44),
    (52 :: Stream(3, 4).map(_ + 50).filter(_ % 2 == 0).toList) -> List(52, 54),
    (62 :: cons(63, Stream(4).map(_ + 60)).filter(_ % 2 == 0).toList) -> List(62, 64),
    ones.take(3).toList -> List(1, 1, 1),
    constant('a').take(3).toList -> List('a', 'a', 'a'),
    from(10).take(3).toList -> List(10, 11, 12),
    fibs.take(7).toList -> List(0, 1, 1, 2, 3, 5, 8),
    fibs2.take(9).toList -> List(0, 1, 1, 2, 3, 5, 8, 13, 21)
  )

  for ((actual, expected) <- data) {
    it should s"match expected $expected" in {
      actual should be(expected)
    }
  }
}
