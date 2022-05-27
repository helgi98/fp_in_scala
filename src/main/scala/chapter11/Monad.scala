package chapter11

import chapter12.{Applicative, Traverse}
import chapter3._
import chapter4._
import chapter5._
import chapter6.State
import chapter7.Nonblocking.Par
import chapter8.Gen
import chapter9.Parsers

import scala.language.higherKinds

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id(f(value))
}


trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(x => x)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft[F[List[B]]](unit(Nil))((acc, x) => map2(f(x), acc)(_ :: _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(sequence(ms.map(a => map2(unit(a), f(a))((_, b) => if (b) List(a) else Nil))))(ls => ls.flatMap(x => x))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(x => x)

  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(a => f(a)))

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(b => g(b)))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    traverseMap(ofa)(x => x)

  def traverseMap[K, V1, V2](ofa: Map[K, V1])(f: V1 => F[V2]): F[Map[K, V2]] =
    ofa.foldLeft[F[Map[K, V2]]](unit(Map.empty))((acc, kv1) => map2(acc, f(kv1._2))((m, v2) => m + (kv1._1 -> v2)))
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](ps: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = ps.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = ps.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- State.get
      _ <- State.set(n + 1)
    } yield (n, a) :: xs).run(0)._1

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
    }
  }


  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
  Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
    override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

    override def flatMap[A, B](ma: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(ma)(ha => G.map(T.traverse(ha)(f))(h => H.join(h)))
  }
}