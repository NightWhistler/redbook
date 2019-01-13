package net.nightwhistler.redbook

object RedbookModule {

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

    def length[A](l: List[A]) = foldLeft(l, 0)((n, a) => n + 1)

    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
      val cl: (B=>B, A) => B => B = { (g, a) => b => g(f(a,b)) }

      foldLeft[A, (B => B)](as, (b:B) => b)( cl )(z)
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = foldLeft[A, List[B]](as, Nil) { (l, a) =>
      flatMap(as)(a => Cons(f(a), Nil))
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldLeft[A, List[B]](as, Nil) { (l, a) =>
      append(l, f(a))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft[A, List[A]](as, Nil) { (l, a) =>
      flatMap(as)( a => if (f(a)) Cons(a, Nil) else Nil)
    }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as -> bs) match {
      case (Cons(a, at), Cons(b, bt)) => Cons(f(a,b), zipWith(at, bt)(f))
      case _ => Nil
    }

    def reverse[A](l: List[A]): List[A] = foldRight[A, List[A]](l, Nil)((a: A, as: List[A]) => append(as, Cons(a, Nil)))

    def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(p, ps), Cons(b, bs)) => (p == b && hasSubSequence(ps, bs)) || hasSubSequence(ps, sub)
    }
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
      case Leaf(a) => l(a)
      case Branch(ll, rr) => b(fold(ll)(l)(b), fold(rr)(l)(b))
    }

    def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)
    def maximum(t: Tree[Int]): Int = fold(t)(i => i)((l, r) => l.max(r))
    def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l.max(r))
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))
  }

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = map (Some(_)) getOrElse ob
    def filter(f: A => Boolean): Option[A] = flatMap (a => if (f(a)) Some(a) else None)
  }

  object Option {
    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      av <- a
      bv <- b
    } yield f(av,bv)

    def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(oa => oa)

    def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = List.foldLeft[A, Option[List[B]]](as, Some(Nil)) { (l, a: A) =>
      l.flatMap(ll => f(a).map(va => List.append(ll, Cons(va,Nil))))
    }

  }

  case object None extends Option[Nothing]
  case class Some[A](get: A) extends Option[A]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m =>
    val variances: Seq[Double] = xs.map(x => math.pow(x - m, 2))
    mean(variances)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}
