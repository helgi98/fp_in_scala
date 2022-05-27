package chapter3

import scala.annotation.tailrec

sealed trait List[+A] {

  def ::[B >: A](el: B): List[B] = Cons(el, this)

  def ++[B >: A](other: List[B]): List[B] = this match {
    case Nil => other
    case Cons(h, t) => Cons(h, t ++ other)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = List.foldRight(this, z)(f)

  def foldLeft[B](z: B)(f: (B, A) => B): B = List.foldLeft(this, z)(f)

  def map[B](f: A => B): List[B] = List.map(this)(f)

  def flatMap[B](f: A => List[B]): List[B] = List.flatMap(this)(f)
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tailOf[T](ls: List[T]): Option[List[T]] = ls match {
    case Cons(_, tail) => Some(tail)
    case Nil => None
  }

  def setHead[T](ls: List[T], head: T): List[T] = ls match {
    case Cons(_, tail) => Cons(head, tail)
    case Nil => sys.error("set head of empty list")
  }

  @tailrec
  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => ls
  }

  @tailrec
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => ls
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](ls: List[A]): Int = foldRight(ls, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumFL(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productFL(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthFL[A](ls: List[A]): Int = foldLeft(ls, 0)((acc, _) => acc + 1)

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc)((b, a) => f(a, b))

  def foldRightViaFoldLeft_2[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((ls, acc) => appendViaFoldRight(ls, acc))

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Cons(x, t) => Cons(f(x), map(t)(f))
    case Nil => Nil
  }

  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt, f))
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(ah, at), Cons(bh, bt)) if ah == bh => hasSubsequence(at, bt)
    case (Cons(_, at), _) => hasSubsequence(at, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Chapter3 {
  def main(args: Array[String]): Unit = {
    println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(List.foldRightViaFoldLeft_2(List(8, 4, 2, 1), 1.0, (x: Int, acc: Double) => x / acc))
  }
}
