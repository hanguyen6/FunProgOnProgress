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


  def memoizedIsPrime: Int => Boolean = {
    def isPrime(num: Int): Boolean = {
      2 to (num - 1) forall (x => num % x != 0)
    }

    val cache = collection.mutable.Map.empty[Int, Boolean]

    num =>
      cache.getOrElse(num, {
        print(s"\n Calling isPrime since input ${num} hasn't seen before and caching the output")
        cache update(num, isPrime(num))
        cache(num)
      })
  }


  val changeList = new ListBuffer[changes]

  def memoizedCountChange: (Int, List[Int]) => Int = {

    def countChange(money: Int, coins: List[Int]): Int  = {
      println(s"split $money with $coins")
      coins match  {
        case h:: t => {
          if (money > h) countChange(money-h, coins) + countChange(money, t)
          else  if (money == h)
            1 + countChange(money, t)
          else 0 + countChange(money, t)
        }
        case _ => 0
      }
    }

    val cache = mutable.HashMap.empty[(Int, List[Int]), Int]

    (money, coins) => cache.getOrElse((money, coins), {
      println(s"calling countChange since input $money, $coins hasnt seen before ")
      cache update ((money, coins), countChange(money, coins))
      cache((money, coins))
    })
  }

  val countChange = memoizedCountChange
  countChange(4, List(1,2,3,5))







}