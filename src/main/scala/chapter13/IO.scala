package chapter13

import chapter11.Monad

import scala.io.StdIn.readLine

case class Player(name: String, score: Int)

object Player {
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter(p1: Player, p2: Player): IO[Unit] = for {
    _ <- PrintLine("Enter temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}