package chapter9


import chapter9.Types.Parser

import scala.language.higherKinds

sealed trait JSON

case object JNull extends JSON

case class JNumber(get: Double) extends JSON

case class JString(get: String) extends JSON

case class JBool(get: Boolean) extends JSON

case class JArray(get: IndexedSeq[JSON]) extends JSON

case class JObject(get: Map[String, JSON]) extends JSON

object JSON {
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def strToToken(s: String): Parser[String] = token(P.string(s))

    def jnumber: Parser[JNumber] = double.map(JNumber)

    def jbool: Parser[JBool] = boolean.map(JBool)

    def jnull: Parser[JSON] = "null".as(JNull)

    def jstring: Parser[JString] = escapedQuoted.map(JString)

    def lit: Parser[JSON] = (jstring | jnumber | jbool | jnull).label("expected literal")

    def jarray: Parser[JArray] = surround("[", "]")(value sepBy "," map (x => JArray(x.toIndexedSeq)))

    def keyval: Parser[(String, JSON)] = escapedQuoted ** (":" *> value)

    def jobj: Parser[JObject] = surround("{", "}")(keyval sepBy "," map (kvl => JObject(kvl.toMap)))

    def value: Parser[JSON] = (lit | jobj | jarray).label("expected json value")

    root(jobj | jarray)
  }
}

/**
 * JSON parsing example.
 */
object JSONExample {

  val jsonTxt =
    """
{
  "Company name" : true,
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG"     ]
}
"""

  val malformedJson1 =
    """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 =
    """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def main(args: Array[String]): Unit = {
    import ReferenceParsers._
    val parser: Parser[JSON] = JSON.jsonParser(ReferenceParsers)
    println(parser.run(malformedJson2))
  }
}