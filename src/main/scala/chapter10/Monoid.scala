package chapter10

import chapter3.{Branch, Leaf, Tree}
import chapter7.Nonblocking.Par
import chapter7.Nonblocking.Par.toParOps
import chapter8.{Gen, Prop}

import scala.language.higherKinds

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f compose g

    override def zero: A => A = a => a
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => b.op(a1(a), a2(a))

    override def zero: A => B = _ => b.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen)(a => m.op(a, m.zero) == a) &&
      Prop.forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z))(p => m.op(m.op(p._1, p._2), p._3) == m.op(p._1, m.op(p._2, p._3)))
  }

  def foldMap[A, B](as: Seq[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: Seq[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(sq => foldMapV(sq, par(m))(b => Par.lazyUnit(b)))

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(l: Option[(Int, Int, Boolean)], r: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (l, r) match {
          case (Some((lmin, lmax, lordered)), Some((rmin, rmax, rordered))) =>
            Some((lmin, rmax, lordered && rordered && lmax <= rmin))
          case (x, None) => x
          case (None, x) => x
        }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(ints, m)(x => Some((x, x, true))).forall(_._3)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(ac), Stub(bc)) => Stub(ac + bc)
      case (Stub(chars), Part(lStub, words, rStub)) => Part(chars + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(chars)) => Part(lStub, words, rStub + chars)
      case (Part(al, aw, ar), Part(bl, bw, br)) => Part(al, aw + (if ((ar + bl).isEmpty) 0 else 1) + bw, br)
    }

    override def zero: WC = Stub("")
  }

  def countWords(s: String): Int = {
    def unstub(s: String): Int = if (s.isEmpty) 0 else 1

    foldMapV(s, wcMonoid)(c => {
      if (c.isWhitespace) wcMonoid.zero else Stub(c.toString)
    }) match {
      case Stub(chars) => unstub(chars)
      case Part(l, wc, r) => unstub(l) + wc + unstub(r)
    }
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])((a, ls) => a :: ls)
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      Monoid.foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[LazyList] {
    override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(v) => f(v, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(v) => f(z, v)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }
  }


  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.map(x => f(x, z)).getOrElse(z)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.map(x => f(z, x)).getOrElse(z)
  }
}
