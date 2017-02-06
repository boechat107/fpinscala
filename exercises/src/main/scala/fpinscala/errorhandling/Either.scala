package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  // ==== My code starts here ====
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(x) => Right(f(x))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case x => x
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      b <- b
      a <- this
    } yield f(a, b)

  // ==== ends here ====
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // ==== My code begins ====
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case x :: xs => for {
        y <- f(x)
        ys <- traverse(xs)(f)
      } yield y :: ys
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  // ==== My code ends ====

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}


// case class Person(name: Name, age: Age)
// sealed class Name(val value: String)
// sealed class Age(val value: Int)

// def mkName(name: String): Either[String, Name] =
//   if (name == "" || name == null) Left("Name is empty.")
//   else Right(new Name(name))

// def mkAge(age: Int): Either[String, Age] =
//   if (age < 0) Left("Age is out of range.")
//   else Right(new Age(age))

// def mkPerson(name: String, age: Int): Either[String, Person] =
//   mkName(name).map2(mkAge(age))(Person(_, _))

// Either.sequence(List(Right(10), Right(20), Left("error"), Right(30)))

// Either.traverse(List(10, 1, 2, 1, 0, 1))(x => Either.safeDiv(10, x))
