object StringOps {

  def isPalindrome(str: String): Boolean = {
    if (str.size <=1 ) true
    else str(0) == str(str.size-1) && isPalindrome(str.slice(1, -1))

  }

}

import StringOps._

val testSt = "DrmrD"
println(isPalindrome(testSt))

