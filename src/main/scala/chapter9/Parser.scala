package chapter9

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  /**
   * Display collapsed error stack - any adjacent stack elements with the
   * same location are combined on one line. For the bottommost error, we
   * display the full line, with a caret pointing to the column of the error.
   * Example:
   * 1.1 file 'companies.json'; array
   * 5.1 object
   * 5.2 key-value
   * 5.10 ':'
   * { "MSFT" ; 24,
   */
  override def toString: String =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""

  def columnCaret: String = (" " * (col - 1)) + "^"
}

trait Parsers[Parser[+_]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a flatMap (f andThen succeed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def surround[A](l: Parser[String], r: Parser[String])(p: => Parser[A]): Parser[A] =
    l *> p <* r

  def sepBy[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] =
    sepBy1(p, s) | succeed(List())

  def sepBy1[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] =
    map2(p, many(s *> p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def whitespace: Parser[String] = "\\s*".r

  def attempt[A](p: Parser[A]): Parser[A]

  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def booleanToken: Parser[String] = token("true|false".r)

  def boolean: Parser[Boolean] = booleanToken.map(_.toBoolean).label("expected boolean")

  def doubleToken: Parser[String] = token("-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?".r)

  def double: Parser[Double] = doubleToken.map(_.toDouble).label("expected number")

  def escapedQuoted: Parser[String] =
    token("(\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\")".r).map(_.drop(1).dropRight(1)).label("expected quoted string")

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] = whitespace *> p <* eof

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def **[B](another: => Parser[B]): Parser[(A, B)] = self.product(p, another)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def sepBy(s: Parser[String]): Parser[List[A]] = self.sepBy(p, s)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def run(s: String): Either[ParseError, A] = self.run(p)(s)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }
}