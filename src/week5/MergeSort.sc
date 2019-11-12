
object  mergeSort {

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match  {
    case (Nil, ys ) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1) => if (x < y) x :: merge (xs1, ys) else  y :: merge(xs, ys1)
  }


  def mSort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n==0) xs
    else {
      val (fst, snd ) = xs splitAt n
      merge(mSort(fst), mSort(snd))
    }
  }

  val nums = List(3,-3, 3,2,-3)
  mSort(nums)

  def mergeElem(x: Int, ys: List[Int]): List[Int] =  ys match  {
    case Nil => List(x)
    case y::ys1 => if (x <= y) x :: ys else mergeElem(y,x::ys1)
  }

  def sortedMerge(xs: List[Int], ys: List[Int]): List[Int] =
    xs match  {
      case Nil => ys
      case x::xs1 => {
        val ongoingList = mergeElem(x, ys)
        sortedMerge(xs1, ongoingList)
      }
    }



  val sortedList1 = List(-2,3,5,6)
  val sortedList2 = List(4,6,7,8)
  sortedMerge(sortedList1, sortedList2)
  merge(sortedList1, sortedList2)

}


