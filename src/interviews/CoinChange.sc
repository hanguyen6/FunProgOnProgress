import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.mutable.HashMap

object CoinChange {

  type changes = List[Long]
  val usedChanges: mutable.HashSet[Long] = mutable.HashSet.empty
  var wayNum = 0;
  def getWays(money: Int, coins: Array[Long], acc: changes): Unit = {
    if (money == 0) {
      println("changes:" + acc.sorted)
      acc.foreach(coin => usedChanges += coin)
      println("used: " + usedChanges)
      wayNum += 1
    }
    else if (money > 0 && !coins.isEmpty) {
      for (coin <- coins if !usedChanges.contains(coin)) {
        getWays(money-coin.toInt, coins, coin :: acc)
      }
    }
  }

  getWays(10, Array(2,3,5,6), Nil)
  wayNum



/*
  val changeList = new ListBuffer[changes]
  def countChange(money: Int, coins: List[Int], acc: List[Int]): Int  = {
    if (money ==0) {
      changeList += acc
      1
    }
    else if (money > 0 && !coins.isEmpty)
      countChange(money-coins.head, coins, coins.head :: acc) + countChange(money, coins.tail, acc)
    else 0
  }

  countChange(3, List(1,2,3,5), Nil)
  changeList.foreach(println)
*/




}