package chapter12

import chapter10.Monoid
import chapter10.Monoid.Foldable
import chapter11.{Functor, Monad}
import chapter12.Traverse.Const
import chapter6.State

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    map2(fa, map2(fb, fc)((b, c) => (b, c)))((a, bc) => f(a, bc._1, bc._2))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({
    type f[x] = (F[x], G[x])
  })#f] {
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (Applicative.this.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

    override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({
    type f[x] = F[G[x]]
  })#f] {
    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      Applicative.this.map2(fa, fb)(G.map2(_, _)(f))

    override def unit[A](a: => A): F[G[A]] = Applicative.this.unit(G.unit(a))
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
    type f[x] = Either[E, x]
  })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
  }

  def validationMonad[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({
    type f[x] = Validation[E, x]
  })#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, t1 ++ (e2 +: t2))
      case (Success(_), e@Failure(_, _)) => e
      case (e@Failure(_, _), Success(_)) => e
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](
      as)(f)(Traverse.monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, Nil: List[A])((a, as) => ((), a :: as))._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  def foldLeftViaMapAccum[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  def fuse[M[_], N[_], A, B](fa: F[A])(f: A => M[B], g: A => N[B])
                            (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_] : Applicative, A, B](fga: F[G[A]])(f: A => M[B]) =
        self.traverse(fga)(ga => G.traverse(ga)(f))
    }
}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[M[_], A, B](fa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      fa match {
        case Some(v) => M.map(f(v))(Some(_))
        case None => M.unit(None)
      }
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({
    type f[x] = Const[M, x]
  })#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }


  def main(args: Array[String]): Unit = {
  }
}

trait Monad2[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

}


trait Applicative2[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

}