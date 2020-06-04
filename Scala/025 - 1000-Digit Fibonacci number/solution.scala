package euler

object Solution extends App {

    val resultLookup = scala.collection.mutable.Map(BigInt(1) -> BigInt(1), BigInt(2) -> BigInt(1))
    
    def fib(n: BigInt): BigInt = {
        if (resultLookup.contains(n)) resultLookup(n)
        else {
            val result = fib(n - 1) + fib(n - 2)
            resultLookup(n) = result
            result
        }
    }
    
    def getDigitCount(n: BigInt): Int = n.toString.length
    
    val first = Stream.from(1)
        .map(x => (x, fib(x)))
        .map(p => (p._1, getDigitCount(p._2)))
        .filter(p => p._2 >= 1000)
        .map(p => p._1)
        .head
    println(first)
}
