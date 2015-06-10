package euler

object Solution extends App {

    def getPalindromes(digits: Int): List[List[Int]] = {
        def impl(digits: Int, isOuter: Boolean): List[List[Int]] = {
            if (digits <= 0) Nil :: Nil
            else if (digits == 1)
                (0 :: Nil) :: (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: 
                (4 :: Nil) :: (5 :: Nil) :: (6 :: Nil) :: 
                (7 :: Nil) :: (8 :: Nil) :: (9 :: Nil) :: Nil
            else {
                val start = if (isOuter) 1 else 0
                val palindromes = for (n <- start to 9; inner <- impl(digits - 2, false)) yield {
                    (n :: inner) ::: (n :: Nil)
                }
                palindromes.toList
            }
        }
        impl(digits, true)
    }
    
    def toInt(values: List[Int]): Int = {
        def impl(values: List[Int], soFar: Int): Int = values match {
            case Nil => soFar
            case head :: tail => impl(tail, soFar * 10 + head)
        }
        impl(values, 0)
    }
    
    def isPalindrome(value: String): Boolean = {
        def impl(front: Int, back: Int): Boolean = {
            if (front >= back) true // we made it to the middle, it must be a palindrome
            else if (value(front) != value(back)) false // we found two characters that don't match
            else impl(front + 1, back - 1)
        }
        impl(0, value.length - 1)
    }
    
    val maxN = scala.math.log10(1000000).toInt    
    val sum = (1 to maxN)
        .flatMap(n => getPalindromes(n))
        .map(n => toInt(n))
        .map(n => (n, n.toBinaryString))
        .filter(p => isPalindrome(p._2))
        .map(p => p._1.toLong)
        .sum
    println(sum)
}






