package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // ==== My code starts here ====
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    if (x >= 0) (x, rng2)
    else (x - Int.MinValue, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, rng2) = nonNegativeInt(rng)
    (x.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (xi, rng2) = rng.nextInt
    val (xd, rng3) = double(rng2)
    ((xi, xd), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (xd, rng2) = double(rng)
    val (xi, rng3) = rng2.nextInt
    ((xd, xi), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (x1, rng2) = double(rng)
    val (x2, rng3) = double(rng2)
    val (x3, rng4) = double(rng3)
    ((x1, x2, x3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def recur(rrng: RNG, l: List[Int], c: Int): (List[Int], RNG) =
      if (c > 0) {
        val (x, nextRng) = rrng.nextInt
        recur(nextRng, x :: l, c -1)
      } else (l, rrng)
    recur(rng, Nil, count)

  }

  def doubleByMap: Rand[Double] =
    map(nonNegativeInt)(x => x.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def recur(li: List[Rand[A]], lo: List[A], rng: RNG): (List[A], RNG) =
      li match {
        case Nil => (lo, rng)
        case h :: t => {
          val (x, nextRng) = h(rng)
          recur(t, x :: lo, nextRng)
        }
      }
    rng => recur(fs, Nil, rng)
  }

  def intsBySequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(RNG.int)).apply(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      val fb = g(a)
      fb(rng2)
    }

  def positiveLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod > 0) rng => (mod, rng)
      else positiveLessThan(n)
    }

  def mapByFlatm[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2ByFlatm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => rng => {
                  val (b, rng2) = rb(rng)
                  (f(a, b), rng2)
                })
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
            val (a, s2) = this.run(s)
            (f(a), s2)
          })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
            val (a, s2) = this.run(s)
            val (b, s3) = sb.run(s2)
            (f(a,b), s3)
          })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
            val (a, s2) = this.run(s)
            f(a).run(s2)
          })
}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[S,B](ls: List[State[S,B]]): State[S,List[B]] = {
    def recur(li: List[State[S,B]], lo: List[B], s: S): (List[B], S) =
      li match {
        case Nil => (lo, s)
        case h :: t => {
          val (b, s2) = h.run(s)
          recur(t, b :: lo, s2)
        }
      }
    State(s => recur(ls, Nil, s))
  }

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def get[S]: State[S,S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def updateState(input: Input)(s: Machine): Machine =
    (input, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, ca, co)) => Machine(false, ca, co+1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca-1, co)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      ss <- State.sequence(
        inputs.map(i => State.modify[Machine](updateState(i)))
      )
      // "get" is used as a function of a flatMap call.
      s <- State.get
    } yield (s.coins, s.candies)
}
