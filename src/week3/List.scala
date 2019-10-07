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

  def nth(elem: Int, aList: List[T]): T  = {
    if (aList.isEmpty) throw new IndexOutOfBoundsException("outside list index")
    else if (elem == 0) aList.head
    else nth(elem-1, aList.tail)
  }
}

