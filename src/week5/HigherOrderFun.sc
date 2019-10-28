
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

  def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _)
  def product(xs: List[Int]): Int = (1 :: xs) reduceLeft( _ * _)


  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( ??? )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( ??? )


  def reduceLeft[T,U](xs: List[T], op: (T, U) => U): U = xs match  {
    case Nil => throw new Error("Nil.reduceLeft")
    case x :: Nil => x
    case x :: xs1 =>  foldLeft(xs1, x)(op)
  }

  def foldLeft[U,T](z: U)(xs: List[T], op: (U,T) => U): U = xs match  {
    case Nil => z
    case x :: xs1 => foldLeft(z)(xs1, op(z, x))
  }

  val nums = List (1,-3, 4,5)
  nums takeWhile(x => x > 0)
  nums dropWhile(x => x > 0)
  nums span  ( x => x > 0)
  nums partition(x =>  x > 0)
  sum(nums)
  product(nums)

  val data = List("a", "a", "a", "b", "c", "c", "a")
  squareList(List(4,2,4))
  pack(data)
  encode(data)
  encode2(data)

}