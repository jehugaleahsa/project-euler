package euler

object Solution extends App {

    def findUpperLimit(exponent: Int): Int = {
        val maxPerDigit = scala.math.pow(9, exponent)
        
        def find(next: Int, digits: Int): Int = {
            if (next <= maxPerDigit * digits) find(next * 10, digits + 1)
            else next
        }
        
        find(10, 2)
    }

    def getDigits(value: Int): List[Int] = {
        if (value == 0) Nil
        else value % 10 :: getDigits(value / 10)
    }
    
    def pow(values: List[Int], exponent: Int): List[Int] = values match {
        case Nil => Nil
        case head :: tail => scala.math.pow(head, exponent).toInt :: pow(tail, exponent)
    }
    
    val exponent = 5
    val limit = findUpperLimit(exponent)
    
    val sum = (2 until limit)
        .map(x => (x, getDigits(x)))
        .map(p => (p._1, pow(p._2, exponent)))
        .map(p => (p._1, p._2.sum))
        .filter(p => p._1 == p._2)
        .map(p => p._1)
        .sum
    println(sum)
}