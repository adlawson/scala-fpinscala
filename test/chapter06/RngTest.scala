package com.adlawson.fpinscala.chapter06

import Rng._
import org.scalatest.{Matchers, FlatSpec}

class RngTest extends FlatSpec with Matchers {
  val rng1 = Simple(42)
  val (n1, rng2) = rng1.nextInt
  val (n2, rng3) = rng2.nextInt

  rng1.toString should "always generate number 16159453" in {
    val (n1a, _) = rng1.nextInt
    (n1 == n1a) should be(true)
    n1 should be(16159453)
  }

  it should "always generate state Simple(1059025964525)" in {
    val (_, rng2a) = rng1.nextInt
    (rng2 == rng2a) should be(true)
    rng2 should be(Simple(1059025964525L))
  }

  rng2.toString should "always generate number -1281479697" in {
    val (n2a, _) = rng2.nextInt
    (n2 == n2a) should be(true)
    n2 should be(-1281479697)
  }

  it should "always generate state Simple(197491923327988)" in {
    val (_, rng3a) = rng2.nextInt
    (rng3 == rng3a) should be(true)
    rng3 should be(Simple(197491923327988L))
  }

  "nonNegativeInt" should "generate a non-negative integer" in {
    val (n, _) = nonNegativeInt(rng2) // typically generates a negative number on next
    n should be(1770001318)
  }

  "double" should "generate a double" in {
    val (d, _) = double(rng1)
    d should be(16159453.toDouble)
  }
}
