package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil        => Nil
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil        => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) {
      l
    } else {
      l match {
        case Nil                 => Nil
        case Cons(_, t) if n > 0 => drop(t, n - 1)
        case _                   => l
      }
    }

  def drop2[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil                 => Nil
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _                   => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, t) =>
        t match {
          case Nil => Nil
          case _   => Cons(h, init(t))
        }
      case _ => Nil
    }

  def init2[A](l: List[A]): List[A] =
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => 1 + b)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumWithFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productWithFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((z: List[A], r: A) => Cons(r, z))

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(a, b))(z)

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(a, b))(z)

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, List[B]())((b: List[B], a: A) => Cons(f(a), b))

  def main(args: Array[String]): Unit = {
    println(x) // x == 3

    println(tail(List(1, 2, 3, 4, 5)))

    println(setHead(List(1, 2, 3, 4, 5), 6))

    println(drop(List(1, 2, 3, 4, 5), 3))
    println(drop2(List(1, 2, 3, 4, 5), 3))

    println(dropWhile(List(1, 2, 3, 4, 5), (_: Int) < 3))

    println(init(List(1, 2, 3, 4, 5)))
    println(init2(List(1, 2, 3, 4, 5)))

    println(length(List(1, 2, 3, 4, 5)))

    println(foldLeft(List(1, 2, 3, 4, 5), 6)((a, b) => a + b))

    println(sumWithFoldLeft(List(1, 2, 3, 4, 5)))

    println(productWithFoldLeft(List(1, 2, 3, 4, 5)))

    println(reverse(List(1, 2, 3, 4, 5)))
  }

}
