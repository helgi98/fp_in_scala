package chapter4


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this.flatMap(a => Some(f(a)))

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some(x) else None)

  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Some(x) => p(x)
    case None => true
  }
}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def sequenceViaFold[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((o, ls) => map2(o, ls)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((x, ls) => map2(f(x), ls)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def main(args: Array[String]): Unit = {
  }
}
