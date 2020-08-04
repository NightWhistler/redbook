package net.nightwhistler.redbook

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)

    def many[A] = self.many(p)
    def slice[A] = self.slice(p)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
  }

  object Laws {
    import Prop.forAll
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s)== run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    //For all inputs, the composite succeeds iff both a and b succeed
    //For all inputs, the output should equal the tupled output of both
    //TODO: Is this correct? Shouldn't we run p2 on whatever is left of the input after p1 consumes it??
    def mapProduct[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in){ s =>
        run(p1 ** p2)(s) == (run(p1)(s), run(p2)(s))
      }

  }

  implicit def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def string(s: String): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed[List[A]](Nil)
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(Nil)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => p2.map(b => (a,b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => p2.map(b => f(a,b)))

  def surround[A](left: Char, body: => Parser[A], right: Char): Parser[A] =
    (char(left) ** body ** char(right)) map (s => s._1._2)
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
