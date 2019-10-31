object stockProfit {

  /** given a stock with prices through several day
    * maximum profit by buying and selling a stock
    */

  // List all possible options. O(n2)
  def maxProfitV1(xs: Array[Int]) = {


    val options: Array[(Int, Int)] =
      for {buy <- xs
           sell <- xs
            if (buy < sell && xs.indexOf(buy) <=xs.indexOf(sell))
      }
      yield (buy, sell)

    // Find profit for each option
    val optionProfit = for (option <- options) yield (option, (option._2 - option._1))

    // Sort by maximum profit
    optionProfit.sortBy(_._2)(Ordering[Int].reverse)
  }

  // for each day's price assume it's highest prices get min of selling prices days before

  def maxProfitV3(prices: Array[Int]): Int = {
    if (prices.length==1) prices(0)
    else {
      (for { sellingPrice <- prices
            sellingDay = prices.indexOf(sellingPrice)
            buyingPrice = prices.take(sellingDay+1).min
            profit = sellingPrice - buyingPrice
          }
         yield profit).max
    }
  }

  val apple = Array(350, 250, 245, 567, 345, 453, 450)

  maxProfitV1(apple)
  maxProfitV3(apple)

}