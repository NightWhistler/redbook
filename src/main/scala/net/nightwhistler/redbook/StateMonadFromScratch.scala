package net.nightwhistler.redbook

object StateMonadFromScratch {

  case class State[A, S](run: S => (A,S)) {
    import State._
    def flatMap[B](f: A => State[B, S]): State[B,S] = State{ s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

    def map[B](f: A => B): State[B, S] = flatMap((unit[B, S] _) compose f)
  }

  object State {
    def unit[A, S](a: A): State[A, S] = State( s => (a, s))

    def doubleInt: State[Unit, Int] = State(s => ((), s * 2))
    def add(i: Int): State[Unit, Int] = State(s => ((), s + 1))

    def get[S]: State[S, S] = State(s => (s, s))
  }

}
