
object HigherOrderFun {

  // Map //
  def map[T, U](xs: List[T], f: T => U): List[U] = xs match {
    case List() => List()
    case x :: xs1 => f(x) :: map(xs1, f)
  }

  def squareList(xs: List[Int]): List[Int] = map[Int, Int](xs, x => x * x)


  def pack[T](xs: List[T]): List[List[T]] = xs match  {
    case Nil => Nil
    case x :: xs1 => {
        val (first, rest ) = xs span(x1 => x1 == x)
      first :: pack(rest)
    }
  }

  // Running length encoding
  def encode[T](xs: List[T]): List[(T, Int)] = xs match  {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest ) = xs span (x1 => x1 == x)
      (first.head, first.length) :: encode(rest)
    }
  }

  def encode2[T](xs: List[T]): List[(T, Int)] =  pack(xs) map (ys => (ys.head, ys.length))


  def foldLeft  = ???
  def foldRight = ???
  def concat = ???
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( ??? )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( ??? )


  squareList(List(4,2,4))

  val nums = List (1,-3, 4,5)
  nums takeWhile(x => x > 0)
  nums dropWhile(x => x > 0)
  nums span  ( x => x > 0)
  nums partition(x =>  x > 0)

  val data = List("a", "a", "a", "b", "c", "c", "a")

  pack(data)
  encode(data)
  encode2(data)

}