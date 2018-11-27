import cats._
import cats.implicits._
import atto._
import Atto._

import scala.collection.GenTraversableOnce

// based on https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
object BoredLisp extends App{
  val symbol: Parser[Char] = oneOf("!#$%&|*+-/:<=>?@^_~")
  val spaces: Parser[Unit] = skipMany1(spaceChar)

  sealed trait LispVal
  case class LAtom(value: String) extends LispVal
  object LAtom{
    def apply(chars: GenTraversableOnce[Char]): LAtom = LAtom(chars.mkString)
  }
  lazy val parseAtom: Parser[LispVal] = for {
    head <- letter | symbol
    tail <- many(letter | symbol | digit)
  } yield head + tail.mkString match {
    case "#t" => LTrue
    case "#f" => LFalse
    case atom @ _ => LAtom(atom)
  }

  case class LString(value: String) extends LispVal
  object LString{
    def apply(chars: GenTraversableOnce[Char]): LString = LString(chars.mkString)
  }
  lazy val parseString: Parser[LispVal] = for { // "string"
    _ <- char('"') // read the first quote
    contents <- many(noneOf("\""))      // read the string body, everything but " is valid
    _ <- char('"')
  } yield LString(contents)

  case class LInt(value: Int) extends LispVal
  val parseInt: Parser[LispVal] = int.map(LInt)

  lazy val parseQuoteSugar : Parser[LispVal] = for { // 'x <=> (quote x)
    _ <- char('\'')
    x <- parseExpr
  } yield LList(LAtom("quote"), x)

  case class LList(value: List[LispVal]) extends LispVal
  object LList{
    def apply[A <: LispVal](values: A*): LList = LList(values.toList)
  }
  lazy val parseList: Parser[LispVal] = sepBy(parseExpr, many1(spaceChar)).map(LList.apply)

  case class LDotList(value: List[LispVal], tail: LispVal) extends LispVal // (a b n . c)
  lazy val parseDotList: Parser[LispVal] = for {
    value <- sepBy(parseExpr, spaces)
    tail <- spaces >> char('.') >> spaces >> parseExpr
  } yield LDotList(value, tail)

  case class LVector(value: Array[LispVal]) extends LispVal
  lazy val parseVector: Parser[LispVal] = for {
    _ <- char('#') >> char('(')
    values <- sepBy(parseExpr, many1(spaceChar))
    _ <- char(')')
  } yield LVector(values.toArray)

  lazy val parseListLike: Parser[LispVal] = char('(') ~> (parseDotList | parseList) <~ char(')')



  sealed abstract class LBoolean(val value: Boolean) extends LispVal
  case object LTrue extends LBoolean(true)
  case object LFalse extends LBoolean(false)

  lazy val parseExpr = parseAtom | parseInt | parseString | parseQuoteSugar | parseListLike | parseVector

  def readExpr(str: String): String = parseExpr.parse(str).done match {
    case ParseResult.Done(tail, result) =>
      s"Found value $result"
    case ParseResult.Fail(tail, stack, message) =>
      stack.fold(s"""Failed parsing at "$tail" with the message "$message"\nStack:\n""")(_ ++ _)
    case ParseResult.Partial(_) => ???
  }


  for {
    arg <- args
  } yield {
    println(readExpr(arg))
  }
}