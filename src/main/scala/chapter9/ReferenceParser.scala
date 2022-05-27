package chapter9

import chapter9.Types.Parser

import scala.language.implicitConversions
import scala.util.matching.Regex

case class ParseState(loc: Location) {
  def advanceBy(numChars: Int): ParseState =
    copy(loc = loc.advanceBy(numChars))

  def input: String = loc.input.substring(loc.offset)

  def slice(n: Int): String = loc.input.substring(loc.offset, loc.offset + n)
}

sealed trait Result[+A] {
  def extract: Either[ParseError, A] = this match {
    case Failure(e, _) => Left(e)
    case Success(a, _) => Right(a)
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) =>
      Failure(e, isCommitted = false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) =>
      Failure(e, c || isCommitted)
    case _ => this
  }

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }
}

case class Success[+A](get: A, length: Int) extends Result[A]

case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

object Types {
  type Parser[+A] = ParseState => Result[A]
}

object ReferenceParsers extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(ParseState(Location(input))) match {
    case Success(x, _) => Right(x)
    case Failure(err, _) => Left(err)
  }

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] = s => f(s) match {
    case Success(a, n) =>
      g(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
    case r: Failure => r
  }

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = s => p1(s) match {
    case Failure(_, false) => p2(s)
    case r => r
  }

  override def slice[A](p: Parser[A]): Parser[String] = {
    s => {
      p(s) match {
        case Success(_, n) => Success(s.slice(n), n)
        case f: Failure => f
      }
    }
  }

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def firstNonMatching(s: String, w: String): Int = {
    w.zipWithIndex.find(_ match {
      case (c, ind) if ind >= s.length || s.charAt(ind) != c => true
      case _ => false
    }) match {
      case None => -1
      case Some((_, ind)) => ind
    }
  }

  override implicit def string(w: String): Parser[String] = {
    s => {
      val i = firstNonMatching(s.input, w)
      if (i == -1)
        Success(w, w.length)
      else Failure(s.loc.advanceBy(i).toError(f"'$w'"), i != 0)
    }
  }

  override implicit def regex(r: Regex): Parser[String] = {
    s => {
      r.findPrefixOf(s.input) match {
        case Some(x) => Success(x, x.length)
        case None => Failure(s.loc.toError(f"regex: $r"), isCommitted = false)
      }
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc, msg))
}