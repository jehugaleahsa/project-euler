package euler

object Solution extends App {
    
    def isPalindrome(value: Int) : Boolean = {
        
        def getDigits(value: Int, digits: List[Int]) : List[Int] = value match {
            case 0 => digits
            case _ => (value % 10) :: getDigits(value / 10, digits)
        }
        
        def isPalindrome(digits: List[Int]) : Boolean = digits == digits.reverse
        
        val digits = getDigits(value, Nil)
        isPalindrome(digits)        
    }
    
    val palindromes = for (i <- 100 until 1000; j <- 100 until 1000 if isPalindrome(i * j)) yield i * j
    println(palindromes.max)
}