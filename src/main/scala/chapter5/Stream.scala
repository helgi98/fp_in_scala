package chapter5

import chapter3._
import chapter5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Empty
  }

  def existsRec(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
    this match {
      case Cons(h, t) => f(t().foldLeft(z)(f), h())
      case _ => z
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a) append acc)

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(x, t) => Some((f(x()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(x, t), n) if n > 0 => Some((x(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(x, t) if f(x()) => Some((x(), t()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h, t), Cons(hh, tt)) => Some(((Some(h()), Some(hh())), (t(), tt())))
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll {
    case (x, y) => x == y
  }

  def tail: Stream[A] = drop(1)

  def tails: Stream[Stream[A]] = this match {
    case Empty => cons(Empty, Empty)
    case s => cons(s, s.tail.tails)
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tailsViaScanRight exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((x, acc) => {
      lazy val accl = acc
      val b2 = f(x, accl._1)
      (b2, cons(b2, accl._2))
    })._2

  def tailsViaScanRight: Stream[Stream[A]] = scanRight(empty[A])((x, acc) => cons(x, acc))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def run(xk: Int, xkn: Int): Stream[Int] =
      cons(xk, run(xkn, xk + xkn))

    run(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(x => Option((x, x)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Option(x, x + 1))

  def fibsViaUnfold: Stream[Int] = unfold((0, 1)) {
    case (xk, xkn) => Some((xk, (xkn, xk + xkn)))
  }
}

object Chapter5 {
  def loggedValue[X](x: X): X = {
    println(s"value {x}")
    x
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).hasSubsequence(Stream(2, 3)))
  }
}