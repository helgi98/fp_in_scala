import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.util.Try

object Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def run(x_k: Int, x_kn: Int, k: Int): Int =
      if (k >= n) x_k
      else run(x_kn, x_k + x_kn, k + 1)

    run(0, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def run(i: Int): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i), as(i - 1))) run(i + 1)
      else false
    }

    run(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    1 :: List(1, 2)
    println("(\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\")".r.findPrefixOf("\"dasda dsa\""))
  }

}
