package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty               => empty
    case Cons(_, _) if n < 1 => empty
    case Cons(h, t)          => cons(h(), t().take(n - 1))
  }

  def take1(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty               => empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t)          => cons(h(), t())
  }

  def drop1(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty      => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def forAll1(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(Option.empty[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def mapWithUnFold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeWithUnFold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)            => Some(h(), (empty, 0))
      case (Cons(h, t), m) if (m > 1) => Some(h(), (t(), m - 1))
      case _                          => None
    }

  def takeWhileWithUnFold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _                      => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _                   => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s) takeWhileWithUnFold (_._2.isDefined) forAll {
      case (Some(h1), Some(h2)) => h1 == h2
    }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty      => List.empty[A] // Nil
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case a     => Some((a, a drop 1))
    } append Stream.empty

//  def scanRight(z: A)(f: (A, A) => A): Stream[A] =
//    unfold(this) {
//      case Empty => None
//      case a     => Some((a.foldRight(z)(f), a drop 1))  // f의 포맷이 맞지 않음
//    } append Stream.empty

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4, 5, 6, 7).take(2).toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7).drop(3).toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(_ > 3).toList)
    println(Stream(1, 2, 3, 4, 5, 6, 7).forAll(_ > 3))
    println(Stream(1, 2, 3, 4, 5, 6, 7).forAll(_ >= 0))
    println(Stream(1, 2, 3, 4, 5, 6, 7).headOption)
    println(Stream().headOption)

    println(
      Stream(1, 2, 3, 4, 5, 6, 7).map(a => a * 2).filter(a => a > 5).toList)
    println(Stream(1, 2, 3, 4).append(Stream(5, 6, 7)).toList)

    println(constant(2).take(5).toList)
    println(from(5).take(5).toList)
    println(fibs.take(10).toList)

    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)

  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, f(b, a + b))
    }
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }
  }

  val onesWithUnFold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantWithUnFold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromWithUnFold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibsWithUnFold: Stream[Int] = unfold((0, 1)) {
    case (s1, s2) => Some(s1, (s2, s1 + s2))
  }

}
