// Avoid computing the tail of a sequence until it is needed for evaluation result

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
xs

/** Create a stream range and avoid computing the cons until it is needed
 *      [][]
 *    /    \
 *   1      [][]
 *          /  \
 *         2    ?
  */
def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >=hi) Stream.empty
else Stream.cons(lo, streamRange(lo+1, hi))

streamRange(10,20)
// find 2nd prime number from the list
def isPrime(n : Int): Boolean = {
  for (factor <- 1 until n) {
    if (n % factor == 0 ) false
  }
  true
}


((1 to 1000).toStream filter isPrime)(1)
