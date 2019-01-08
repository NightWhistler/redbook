package net.nightwhistler.redbook

class RedbookModule {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    def tail[A](as: List[A]) = as match {
      case Nil => Nil
      case Cons(a, as) => as
    }

    def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
