package com.adlawson.fpinscala.chapter06

trait Rng {
  def nextInt: (Int, Rng)
}

object Rng {
  case class Simple(seed: Long) extends Rng {
    // Linear congruential generator
    // http://en.wikipedia.org/wiki/Linear_congruential_generator
    def nextInt: (Int, Rng) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = Simple(newSeed)
      ((newSeed >>> 16).toInt, nextRng)
    }
  }

  @annotation.tailrec
  def nonNegativeInt(rng: Rng): (Int, Rng) = rng.nextInt match {
    case (i, r1) if i >= 0 => (i, r1)
    case (_, r1) => nonNegativeInt(r1)
  }

  // non recursive
  // `-(i + 1)` because `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)` appaz
  def nonNegativeInt2(rng: Rng): (Int, Rng) = {
    val (i, r1) = rng.nextInt
    if (i < 0) (-(i + 1), r1)
    else (i, r1)
  }

  def double(rng: Rng): (Double, Rng) = rng.nextInt match {
    case (i, rng1) => (i.toDouble, rng1)
  }

  def intDouble(rng: Rng): ((Int, Double), Rng) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: Rng): ((Double, Int), Rng) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(r1: Rng): ((Double, Double, Double), Rng) = {
    val (d1, r2) = double(r1)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  def ints(n: Int)(rng: Rng): (List[Int], Rng) = {
    if (n == 0) (List.empty, rng)
    else {
      val (n, r1) = rng.nextInt
      val (ns, r2) = ints(n - 1)(r1)
      (n :: ns, r2)
    }
  }

  type Rand[+A] = Rng => (A, Rng)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(int)(_.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
}
