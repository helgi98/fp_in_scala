package chapter8

import chapter6.{RNG, SimpleRNG, State}
import chapter7.Par
import chapter7.Par.Par
import chapter8.Gen.{listOf, listOf1, unit}
import chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

import java.util.concurrent.{ExecutorService, Executors}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop((n, tc, rng) => {
    this.run(n, tc, rng) match {
      case Passed | Proved => p.run(n, tc, rng)
      case x => x
    }
  })

  def ||(p: Prop): Prop = Prop((n, tc, rng) => {
    this.run(n, tc, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(n, tc, rng)
      case x => x
    }
  })

  def tag(msg: String): Prop = Prop {
    (n, tc, rng) =>
      run(n, tc, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (max, tc, rng) =>
      val casesPerSize = (tc + (max - 1)) / max
      val props: LazyList[Prop] =
        LazyList.from(0).take((tc min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, tc, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, tc, rng) =>
      randomStream(as)(rng).zip(LazyList.from(0)).take(tc).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = this.flatMap(a => unit(f(a)))

  def map2[B, C](sb: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(RNG.intInRange(start, stopExclusive - 1))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def double: Gen[Double] = Gen(RNG.double)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    listOfN(unit(n), g)

  def listOfN[A](nGen: Gen[Int], g: Gen[A]): Gen[List[A]] =
    nGen.flatMap(n => Gen(RNG.sequence((1 to n).map(_ => g.sample))))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    double.flatMap(d => if (d <= g1Threshold) g1._1 else g2._1)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

}

object Chapter8 {
  def main(args: Array[String]): Unit = {

    Prop.run(forAllPar(Gen.choose(-100, 100))(i => {
      Par.equal(Par.map(Par.unit(i))(_ + 1), Par.unit(i + 1))
    }))
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val executorService: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Prop.forAll(executorService ** g) { case (s, a) => f(a)(s).get }


  def checkListSort: Prop = {
    val smallInt = Gen.choose(-100, 100)
    Prop.forAll(listOf(smallInt)) { ns =>
      val sortedNs = ns.sorted
      sortedNs.isEmpty || sortedNs.tail.isEmpty || sortedNs.zip(sortedNs.tail).forall(_ match {
        case (x, y) => x <= y
      })
    }
  }

  def checkListMax: Prop = {
    val smallInt = Gen.choose(-10, 10)
    Prop.forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
  }
}