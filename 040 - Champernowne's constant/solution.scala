package euler

object Solution extends App {
    
    def findValue(offset: Int): Int = {
    
        def getDigitCount(value: Int): Int = scala.math.log10(value).toInt + 1
        
        def getDigit(value: Int, offset: Int): Int = {
            val asString = value.toString
            asString.substring(offset, offset + 1).toInt
        }
    
        def impl(next: Int, count: Int): Int = {
            val digitCount = getDigitCount(next)
            if (digitCount + count > offset) getDigit(next, offset - count)
            else impl(next + 1, count + digitCount)
        }
        
        impl(1, 0)    
    }
    
    val product = (0 to 6)
        .map(p => scala.math.pow(10, p).toInt)
        .map(d => d - 1) // adjust for zero-based index
        .map(d => findValue(d))
        .product
    println(product)
}