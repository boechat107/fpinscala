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

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State (rng => {
                 def recur (r: RNG): (Int, RNG) = {
                   val (i, rng2) = r.nextInt
                   if (i >= start && i < stopExclusive) (i, rng2)
                   else recur(rng2)
                 }
                 recur (rng)}))

  def unit[A](a: => A): Gen[A] = ???
}

case class Gen[A](sample: State[RNG,A])

// trait Gen[A] {
//   def map[A,B](f: A => B): Gen[B] = ???
//   def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
// }

trait SGen[+A] {

}
