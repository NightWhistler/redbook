package net.nightwhistler.redbook

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r)
    case (i, r) if i < 0 => (i * -1, r)
    case o => o
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def _double(rng: RNG): Rand[Double] = map(nonNegativeInt)( _ / (Int.MaxValue.toDouble +1) )

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)( a => map(rb)( b => f(a, b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, r) = f(rng)
    g(a)(r)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s: S =>
    val (a,s2) = run(s)
    f(a).run(s2)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def sequence[S, A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

}

object Candy {

  def applyInput: Input => Machine => Machine = { i => m =>
    i match {
      case _ if m.candies == 0 => m
      case Coin if m.locked => m.copy(locked = false, coins = m.coins +1)
      case Turn if ! m.locked => m.copy(locked = true, candies = m.candies -1)
      case _ => m
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val mods: List[State[Machine, Unit]] = inputs.map( i => State.modify(applyInput(i)))
    val sequenced: State[Machine, List[Unit]] = State.sequence(mods)
    sequenced.flatMap( _ => State.get.map( m => (m.candies, m.coins)))
  }
}
