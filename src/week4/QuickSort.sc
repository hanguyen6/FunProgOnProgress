object  session {


  def insert(x: Int, xs: List[Int]): List[Int] = xs match  {
    case List() => List(x)
    case y:: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
  // Insertion Sort on a List
  def iSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys =>  insert (y, iSort(ys))

  }

  def qSort(a: Array[Int]): Array[Int] = {
    if (a.length <=1) a
    else {
      val pivot = a(a.length / 2)
      Array.concat(
        qSort(a.filter( pivot >)),
        a.filter(pivot ==),
        qSort(a.filter(pivot <)))
    }
  }





  iSort(List(-3,4,2))
  qSort(Array(-3,4,-7, 8,4,3))
  iSort(List())


}