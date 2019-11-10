object  LazyEval {
  def expr = {
    // call by value
    val x = {print{"x"}; 1}
    // lazy val
    lazy val y = {print("y"); 2}
    // call by name
    def z = {print("z"); 3}
    z + y + x + z + y + x
  }
  println(expr)

  def from(n: Int): Stream[Int] = n #:: from(n+1)

  val nats = from(0)
  (nats map(_*4) take 4).toList

  /* Sieve of Eratosthenes : calculate prime number
  * Take first number and eliminate all numbers which are the multiple of the first number
  * Continue the stream with the remaining number
  * */
  def sieve(s: Stream[Int]): Stream[Int]  = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }

  val primes = sieve(from(2))
  primes.take(10).toList

  /**
   * Calculate Square Roots
   *
   */
  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess +  x / guess)/2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  def isGoodEnough(guess: Double, x : Double) =
    math.abs((guess * guess - x)/x) < 0.0001

  sqrtStream(6).filter(isGoodEnough(_, 6))

}