object RLEncoding {

  // Given same string/character , encode it with the char and its positions
  // Find the most packed string
  def RLEnding (array: Array[String]) = {


    def pack(array: Array[String]): Array[(String, Int)] = ???

    val encoded: Array[(String, Int)]= pack(array)
    encoded.maxBy(_._2)(Ordering[Int].reverse)

  }


}