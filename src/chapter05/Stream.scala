package com.adlawson.fpinscala.chapter05

sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List.empty
  }
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // final (or private) required to use tailrec annotation
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def exists(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(f: A => Boolean): Boolean =
    foldRight(false)((a, b) => f(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold(this, n) {
    case (Cons(h, t), _) if n > 1 => Some((h(), (t(), n - 1)))
    case (Cons(h, _), _) if n == 1 => Some((h(), (empty, 0)))
    case _ => None
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // More efficient as it uses the same lazy object
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] =
      cons(x, go(y, x + y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case _ => empty
  }

  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def from2(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n, n + 1)))

  def constant3[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val ones2: Stream[Int] = constant3(1)
}
