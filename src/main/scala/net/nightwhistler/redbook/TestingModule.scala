package net.nightwhistler.redbook

import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object TestingModule {

}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample flatMap (a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.map(RNG.nonNegativeInt)(i => start + i % (stopExclusive - start))))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.nonNegativeInt)(i => i % 2 == 0)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    val doubleGen = Gen(State(RNG.double))
    doubleGen.flatMap( d => if (d < g1Threshold) g1._1 else g2._1)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    import RNG.Rand
    val rands: List[Rand[A]] = List.fill(n)(g.sample.run)
    Gen(State(RNG.sequence(rands)))
  }

}

object Prop {

  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

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
    def isFalsified = false
  }

  import StreamModule._


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (s, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }


}

import Prop._

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

}

// trait Gen[A] {
//   def map[A,B](f: A => B): Gen[B] = ???
//   def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
// }
case class SGen[A](forSize: Int => Gen[A]) {
  // def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i =>
    val ga: Gen[A] = forSize(i)
    val s: SGen[B] => Gen[B] = _.forSize(i)
    ga.flatMap( s compose f)
  }

  // def **[B](s2: SGen[B]): SGen[(A,B)] =
  //   SGen(n => apply(n) ** s2(n))

}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

