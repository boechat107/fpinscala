package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // ======= My code starts here =====
  // Exercise 1
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)
    // this match {
    //   case None => ob
    //   case Some(x) => Some(x)
    // }

  def filter(f: A => Boolean): Option[A] =
    if (this.map(f).getOrElse(false)) this
    else None
  // ===== End of my code ====
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ======= My code starts here =====
  def variance(xs: Seq[Double]): Option[Double] = {
    // mean(xs) match {
    //   case None => None
    //   case Some(m) => Some(xs.map(x => math.pow(x - m, 2)).sum / xs.length)
    // }
    mean(xs).flatMap(m =>
      Some(xs.map(x =>
             math.pow(x - m, 2)).sum / xs.length))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- a
      b <- b
    } yield f(a, b)

  // Signature to be used to implement bothMatch_2 of exercise 4.
  def mkMatcher(pat: String): Option[String => Boolean] = sys.error("todo")

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    // a match {
    //   case Nil => Some(Nil)
    //   case None :: xs => None
    //   case Some(x) :: xs => sequence(xs) match {
    //     case None => None
    //     case Some(ys) => Some(x :: ys)
    //   }
    // }
    a match {
      case Nil => Some(Nil)
        // case x :: xs => x.flatMap(h => sequence(xs).map(h :: _))
      case x :: xs => for {
        x <- x
        xs <- sequence(xs)
      } yield x :: xs
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs => for {
        y <- f(x)
        ys <- traverse(xs)(f)
      } yield y :: ys
    }

  // Rewritting sequence using traverse.
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
