package week3

/**
  * A representation for natural number as a object // Peano Numbers
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat

}

object Zero extends  Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException("Zero.predecessor")
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if (that.isZero) this else throw new Error("Negative number ")

}

class Succ(n : Nat) extends  Nat {
  override def isZero: Boolean = false
  override def +(that: Nat): Nat = new Succ(n + that)
  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def predecessor: Nat = n
}