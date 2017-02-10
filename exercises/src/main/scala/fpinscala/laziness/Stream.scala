package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // === My code starts here ====
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    (this, n) match {
      case (Empty, _) => Empty
      case (_, 0) => Empty
      case (Cons (h, t), _) => Cons(h, () => t().take(n - 1))
    }

  def drop(n: Int): Stream[A] =
    (this, n) match {
      case (Empty, _) => Empty
      case (x, 0) => x
      case (Cons(h, t), _) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p))
                         else Empty
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b)
                                          else Empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b)
                                          else b)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean =
    (this, s) match {
      case (_, Empty) => true
      case (Empty, _) => false
      case (Cons(h0, t0), Cons(h1, t1)) => if (h0() == h1()) t0().startsWith(t1())
                                           else false
    }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (0, _) => None
      case (_, Empty) => None
      case (x, Cons(h, t)) => Some((h(), (n - 1, t())))
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => if (p(h())) Some((h(), t()))
                         else None
    }

  def zip[B](l: Stream[A])(f: (A, A) => B): Stream[B] =
    unfold((this, l)) {
      case (Empty, _) | (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()),
              (t1(), t2())))
    }

  def zipAll[B](l: Stream[A])(f: (Option[A], Option[A]) => B): Stream[B] =
    unfold((this, l)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some( ( f(None, Some(h())),
                                          (Empty, t())) )
      case (Cons(h, t), Empty) => Some( ( f(Some(h()), None),
                                          (t(), Empty)) )
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some( (f(Some(h1()), Some(h2())),
               (t1(), t2())) )
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some( (Cons(h, t), t()) )
      case Empty => None
    }

  // === ends here ===
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // === My code starts here ===
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def recur(n0: Int, n1: Int): Stream[Int] =
      Stream.cons(n0, recur(n1, n0 + n1))
    recur(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  def fibs2: Stream[Int] =
    unfold((0, 1)){ case (n0, n1) => Some((n0, (n1, n0 + n1))) }

  def from2(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(x => Some((x, x)))

  val ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zipAll(s2) {
      case (None, _) | (_, None) => false
      case (Some(x), Some(y)) => x == y
    } exists(!_)

}
