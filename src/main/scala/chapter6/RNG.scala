package chapter6

import chapter6.State.{get, sequence, transit, unit}

trait RNG {
  def nextInt: (Int, RNG)

  def nextDouble: (Double, RNG) = RNG.nonNegativeInt.map(i => i / (Int.MaxValue + 1.0)).run(this)

  def nextBoolean: (Boolean, RNG) = RNG.nonNegativeInt.map(i => i % 2 == 0).run(this)

  def ints(count: Int): (List[Int], RNG) = {
    (1 to count).foldLeft[(List[Int], RNG)]((Nil, this))((acc, _) => {
      val (i, rn) = acc._2.nextInt
      (i :: acc._1, rn)
    })
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] = State.unit(a)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = s.map(f)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue + 1.0))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ra.map2(rb)(f)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: Seq[Rand[A]]): Rand[List[A]] = State.sequence(fs)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (i, next) = rng.nextInt
    (if (i < 0) -(i + 1) else i, next)
  })

  def intInRange(a: Int, b: Int): Rand[Int] =
    map(nonNegativeLessThan(b - a))(_ + a)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f.flatMap(g)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })

  def boolean: Rand[Boolean] = State(rng => rng.nextBoolean)

  def mapViaFM[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ViaFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
