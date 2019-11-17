
import scala.collection.mutable.HashMap
object Anagrams {

  type Occurrences = List[(Char, Int)]

  def cFreq(s: String): Occurrences = {
    s.toLowerCase.toList
      .groupBy(c => c)
      .mapValues(_.size)
      .toList
      .sorted
  }

  def anaNum(n: Int): Int = n * (n-1)/2

  val s = "kkkk"
  val n = s.size



  val subs: HashMap[Occurrences, List[String]] = HashMap.empty

  for {
    i <- 0 until n
    j <- i until n
  }
  {
    val subStr = s.slice(i, j+1)
    val freq = cFreq(subStr)

    println(subStr + ":" + freq)

    if (!subs.contains(freq)) subs += freq -> List(subStr)
    else {
      val anagrams: List[String] = subStr :: subs.get(freq).get
      subs += freq -> anagrams
    }
  }

  subs.filter(freq => freq._2.size > 1)
      .foreach(freq  => println(freq._1 + ":" + freq._2))

  subs.map(freq => freq._2.size)
      .map(anaNum).sum

}