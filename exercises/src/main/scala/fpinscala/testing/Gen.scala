package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

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

case class Prop(run: (TestCases, RNG) => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(gen)(rng).
      zip(Stream.from(0))((a,b) => (a,b)).
      take(n).map {
        case(a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](v: A, e: Exception): String =
    s"test case: $v\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString}"
}

object Gen {
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State (rng => {
                 def recur (r: RNG): (Int, RNG) = {
                   val (i, rng2) = r.nextInt
                   if (i >= start && i < stopExclusive) (i, rng2)
                   else recur(rng2)
                 }
                 recur (rng)}))

  def unit[A](a: => A): Gen[A] =
    Gen(State(rng => (a, rng)))

  def boolean: Gen[Boolean] =
    Gen(State(rng => {
                val (x, rng2) = rng.nextInt
                if (x % 2 == 0) (true, rng2)
                else (false, rng2)
              }))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val ls: List[State[RNG,A]] = (0 to n).toList map (i => g.sample)
    Gen(State.sequence(ls))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    val genChoice = Gen.boolean
    Gen(State(rng => {
                val (c, rng2) = genChoice.sample.run(rng)
                if (c) g1.sample.run(rng2)
                else g2.sample.run(rng2)
              }))
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    // Value to normalize the probabilities into [0,1].
    val totalProb = g1._2 + g2._2
    Gen(State(rng => {
                val (d, rng2) = RNG.double(rng)
                // It's not completely fair, since d can be 1.
                if (d < g1._2 / totalProb) g1._1.sample.run(rng2)
                else g2._1.sample.run(rng2)
              }))
  }

}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State(rng => {
                val (a, rng2) = this.sample.run(rng)
                f(a).sample.run(rng2)
              }))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap(n => Gen.listOfN(n, this))

}

// trait Gen[A] {
//   def map[A,B](f: A => B): Gen[B] = ???
//   def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
// }

trait SGen[+A] {

}
