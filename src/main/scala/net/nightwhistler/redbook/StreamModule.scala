package net.nightwhistler.redbook

object StreamModule {

  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h,_) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n -1))
      case _ => Empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n -1)
      case _ => this
    }

    // def takeWhile(p: A => Boolean): Stream[A] = this match {
    //   case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    //   case _ => this
    // }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

    // def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty )
    def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    // def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
    def map[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

    def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h,t) else t)

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def zipWith[B,C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some((f(hl(), hr()), (tl(), tr())))
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some(((Some(hl()), Some((hr()))), (tl(), tr())))
      case (Empty, Cons(hr, tr)) => Some(((None, Some(hr()))), (Empty, tr()))
      case (Cons(hl, tl), Empty) => Some(((Some(hl()), None)), (tl(), Empty))
      case _ => None
    }

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    def startsWith[A](s: Stream[A]): Boolean = zipWith(s)( (a, b) => a == b ).forAll(b => b)

    def tails: Stream[Stream[A]] = unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((t(), t()))
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

    def from(n: Int): Stream[Int] = unfold(n)(nn => Some(nn, nn +1))

    def fibs = unfold((0,1)){ case (n0, n1) => Some((n0, (n1, n0+n1))) }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))


  }

}
