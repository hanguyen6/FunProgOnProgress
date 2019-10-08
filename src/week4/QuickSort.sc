
object  session {

  def insert(x: Int, xs: List[Int]): List[Int] = xs match  {
    case List() => List(x)
    case y:: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
  def iSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys =>  insert (y, iSort(ys))

  }



  iSort(List(-3,4,2))



}