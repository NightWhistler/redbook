package net.nightwhistler.redbook

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =  map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(Nil)) {
    case (a: Par[A], l: Par[List[A]]) => map2(a, l)( (aa, ll) => aa :: ll )
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val i: Par[List[List[A]]] = parMap(as)( a => if (f(a)) List(a) else (List()) )
    map(i)(_.flatten)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)( choices(_) )

  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => choices(pa(es).get)(es)
  // def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = join(map(pa)(choices))

  // def join[A](a: Par[Par[A]]): Par[A] = es => (a(es).get())(es)
  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(i => i)
}