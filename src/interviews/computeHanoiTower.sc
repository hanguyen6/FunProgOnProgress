import java.util

import scala.collection.mutable.ArrayBuffer

object computeHanoiTower {
  // Steps to compute Hanoi tower:
  /** - Given n rings, need to move rings from peg A -> B using peg C
   * 		+ Move (n-1) rings on top from peg A -> C
		+ Move lowest ring from peg A -> B
	- Now with (n-1) ring, need to move them from peg C -> B using A
		+ Move (n-2) rings on top from C -> A
		+ Move lowest rings from peg C -> B
   */

  // Peg is a stack with pop and put operation
  class Peg {

    var elems: List[Int] = List.empty

    def isEmpty: Boolean = elems.isEmpty

    def pop(): Int = {
      if (!isEmpty) {
        val ret = elems.head
        //println(s"Pop $ret")
        elems = elems.tail
        ret
      } else throw new NullPointerException("Empty stack ")
    }

    def put(elem: Int): Unit = this.elems = elem :: elems

  }

  def computeHanoiTowerSteps(rings2Move: Int, pegs: ArrayBuffer[Peg], fromPeg: Int, toPeg: Int, usePeg: Int, result: ArrayBuffer[List[Int]]): Unit  = {
    if (rings2Move > 0) {
      if (rings2Move-1 > 0 ) println(s"Move ${rings2Move-1} rings from peg ${fromPeg} to ${usePeg} using peg ${toPeg}  ..")
      computeHanoiTowerSteps(rings2Move - 1, pegs , fromPeg, usePeg, toPeg, result)
      //println(s"Put a ring from peg ${fromPeg} to peg ${toPeg} ")
      pegs(toPeg).put(pegs(fromPeg).pop)

      /*println("State now ")
      pegs.foreach(peg => println(s"peg ${pegs.indexOf(peg)} " + peg.elems))*/
      result += (List(fromPeg, toPeg))
      //println(s"Ongoing Steps: $result")
      //if (rings2Move-1 >0) println(s"Moving ${rings2Move-1} rings from peg ${usePeg} to peg ${toPeg} using peg ${fromPeg} ... ")
      computeHanoiTowerSteps(rings2Move-1, pegs, usePeg, toPeg, fromPeg, result)
    }

  }

  def computeHanoiTower(rings2Move: Int) {

    val pegs: ArrayBuffer[Peg] = ArrayBuffer.empty[Peg]
    for (i <- 0 until 3) {
      val peg =  new Peg
      pegs += peg
    }

    // add rings to one peg
    println("Initialize: Adding rings to peg 0")
    for (i <- rings2Move to 1 by -1) {
      println(s"Add ring ${i} to peg 0")
      pegs(0).put(i)
    }

    println(s"Starting from peg 0: with rings as ${pegs(0).elems}")

    // move rings from peg 0 to peg 1 using peg 2
    val result = ArrayBuffer.empty[List[Int]]
    computeHanoiTowerSteps(rings2Move,pegs, 0, 1, 2, result)

    println(s"Steps to move ${rings2Move} rings from peg 0 to peg 1 using peg 2")
    println(result.foreach(println))

    println("Peg states: ")
    pegs.foreach(peg => println(s"peg ${pegs.indexOf(peg)}: " + peg.elems))
  }

  computeHanoiTower(4)







}