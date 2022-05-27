package chapter3


sealed trait Tree[+A] {
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }

  def sizeViaFold: Int = this.fold(_ => 1)(1 + _ + _)

  def depthViaFold: Int = this.fold(_ => 0)(1 + _ max _)

}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def maximumViaFold(t: Tree[Int]): Int = t.fold(x => x)(_ max _)
}
