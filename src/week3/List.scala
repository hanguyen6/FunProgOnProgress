package week3


  trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]

  }

  class Cons[T](val head:T, val tail: List[T]) extends List[T] {
    override def isEmpty: Boolean = false
  }

  class Nil[T] extends List[T] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("Nil.head")

    override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }


object List  {

  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil))
  def apply[T](x1: T): List[T] = new Cons[T](x1, new Nil[T])
  def apply[T]: List[T] = new Nil[T]

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](elem: Int, aList: List[T]): T  = {
    if (aList.isEmpty) throw new IndexOutOfBoundsException("outside list index")
    else if (elem == 0) aList.head
    else nth(elem-1, aList.tail)
  }

  def init[T](xs: List[T]):  List[T] = xs match  {
    case List() => throw new NoSuchElementException("empty list")
    case List(x) => List(x)
    case y :: ys => y::init[T](ys)
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new NoSuchElementException("empty list")
    case List(x) => x
    case x :: ys => last[T](ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match  {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n: Int, xs: List[T]) = xs match  {

  }


}

