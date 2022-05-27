package chapter7

import chapter7.Par._

import java.util.concurrent.{CompletableFuture, ExecutorService, Executors, Future}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => CompletableFuture.completedFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val av = a(es)
    val bv = b(es)
    CompletableFuture.completedFuture(f(av.get, bv.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequenceFL[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(List[A]()))((acc, x) => map2(acc, x)((ls, a) => a :: ls))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    map(sequence(as.map(asyncF(a => if (f(a)) List(a) else Nil))))(_.flatten)
  }

  def equal[A](es: ExecutorService)(x: Par[A], y: Par[A]): Boolean =
    run(es)(x).get() == run(es)(y).get()

  def equal[A](x: Par[A], y: Par[A]): Par[Boolean] = map2(x, y)(_ == _)

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      f(run(es)(pa).get)(es)
    }

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    run(es)(run(es)(a).get)
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(ind => choices(ind))
}

object Chapter7 {

  def checkBalancedParentheses(s: String): (Int, Int) = {
    s.foldLeft((0, 0))((acc, c) => c match {
      case '(' => (acc._1 + 1, acc._2)
      case ')' => if (acc._1 > 0) (acc._1 - 1, acc._2) else (acc._1, acc._2 + 1)
      case _ => acc
    })
  }

  def balancedParentheses(s: String): Boolean = {
    val (open, closed) = checkBalancedParentheses(s)
    open == 0 && closed == 0
  }

  def combineChecks(checkLeft: (Int, Int), checkRight: (Int, Int)): (Int, Int) = {
    val (lo, lc) = checkLeft
    val (ro, rc) = checkRight

    val open = (lo - rc).max(0) + ro
    val close = lc + (rc - lo).max(0)

    (open, close)
  }

  def mapAndReduce[T, K](ls: IndexedSeq[T])(f: T => K)(reducer: (K, K) => K): Par[Option[K]] = {
    def internal(ls: IndexedSeq[T]): Par[List[K]] = {
      if (ls.size <= 1)
        unit(ls.headOption.map(x => List(f(x))).getOrElse(Nil))
      else {
        val (l, r) = ls.splitAt(ls.length / 2)
        map2(fork(internal(l)),
          fork(internal(r)))(_ ++ _)
      }
    }

    map(internal(ls))(_.reduceOption(reducer))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    map(mapAndReduce(ints)(i => i)(_ + _))(_ getOrElse 0)

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
    mapAndReduce(ints)(i => i)(_.max(_))

  def countWords(paragraphs: IndexedSeq[String]): Par[Int] =
    map(mapAndReduce(paragraphs)(_.split("\\s+").length)(_ + _))(_ getOrElse 0)


  def forkN[A](n: Int)(a: Par[A]): Par[A] = (1 to n).foldLeft(a)((acc, _) => fork(acc))


  def main(args: Array[String]): Unit = {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(10)
    println(Par.equal(S)(a, forkN(10)(a)))
  }
}
