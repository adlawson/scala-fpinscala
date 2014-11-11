package com.github.adlawson.fpinscala.chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Companion object to the List type defined above.
// Contains functions that act on the type.
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0 // We don't need to keep going
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // A* denotes A is variadic
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // Again, nobody knows what _ is

  // This syntax relies on companion object `apply` func that accepts the list
  // types (can be variadic too, as with this `apply`)
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  // --- Going rogue

  def sub(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x - sub(xs)
  }

  def divide(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0 // Should throw but I haven't got that far yet
    case Cons(x, xs) => x / divide(xs)
  }

  // --- Back on course

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil // Maybe throw here?
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil // This book really needs to show me how to throw
    case Cons(_, t) => Cons(a, t)
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (0 == n) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // Check the guard on that case
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](as: List[A]): List[A] = as match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t)) // Replacing tail of cons is bad times
  }

  def curriedDropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    dropWhile(as, f)

  // Currying lets type inference determine arg types on `f` (Int, Double, whatever)
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)((x, y) => x + y) // Note x, y don't have defined types

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)((x, y) => x * y)

  def sum2Anon(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ints: List[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_*_)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((b, a) => f(a, b))

  // I had to get this one from the answers. This is crazy awesome.
  // Just to show I'm not just copying down answers without understanding
  // what's going on, here's my explanation:
  // The accumulator of `foldLeft` is a function that gets wrapped up in a new
  // closure all the way down the list. Then the closure stack is unwound to
  // evaluate the true value of the fold.
  // Inception reduction.
  def foldRight3[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeft3[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, List[A]())(append2)

  def add1(as: List[Int]): List[Int] =
    foldLeft(as, List[Int]())((acc, h) => Cons(h+1, acc))

  def dsToString(as: List[Double]): List[String] =
    foldLeft(as, List[String]())((acc, h) => Cons(h.toString, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, List[B]())((acc, h) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, List[A]())((acc, h) =>
      if (f(h)) Cons(h, acc)
      else acc
    ) // Could be all on one line

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def sumPair(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, sumPair(t1, t2))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // Looking at the answer, they wanted to do work on mixes types, so here it is
  def zipWith2[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith2(t1, t2)(f))
  }

  // Totally rubbish implementation. Only checks the first 2 elements. Not what I want
  //def isHead[A](as: List[A], head: List[A]): Boolean = (as, head) match {
    //case (Cons(a1, b1, t2), Cons(a2, b2, t2)) if a1 == a2 => b1 == b2
    //case _ => false
  //}

  def isHead[A](as: List[A], head: List[A]): Boolean = (as, head) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => isHead(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
