package week3

object session {

  singleton(1)
  singleton(true)
  nth(1, new Cons[Int](1, new Cons[Int](2, new Nil)))


  def flatten(xs: List[Any]): List[Any] = ???
  flatten(List(List(1,2), 2, List(4, List(4,8))))

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match  {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case ( x :: xs1, y :: ys1) => if (x < y) x :: merge (xs1, ys) else y :: merge (xs , ys1)
  }
}
