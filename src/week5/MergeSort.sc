
object  mergeSort {

  def mSort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n==0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match  {
        case (Nil, ys ) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge (xs1, ys) else  y :: merge(xs, ys1)
      }
      val (fst, snd ) = xs splitAt n
      merge(mSort(fst), mSort(snd))
    }
  }

  val nums = List(3,-3, 3,2,-3)
  mSort(nums)


}


