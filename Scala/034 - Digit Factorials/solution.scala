package euler

class FactorialHelper {

    private[this] val resultLookup = scala.collection.mutable.Map[BigInt, BigInt]()
    
    private[this] def store(value: BigInt, result: BigInt): BigInt = {
        resultLookup(value) = result
        result
    }
    
    def factorial(value: BigInt): BigInt = {
    
        @scala.annotation.tailrec
        def impl(value: BigInt, accum: BigInt => BigInt): BigInt = {
            if (resultLookup.contains(value)) accum(resultLookup(value))
            else if (value <= 1) accum(store(value, 1))
            else impl(value - 1, p => accum(store(value, value * p)))
        }
        
        impl(value, p => p)
	}
}

object Solution extends App {

    def getDigits(value: BigInt): List[Int] = {
		if (value == 0) Nil
		else (value % 10).toInt :: getDigits(value / 10)
	}
    
    val helper = new FactorialHelper
    
    val sum = Stream.from(3)
        .map(n => (n, getDigits(n)))
        .map(p => (p._1, p._2.map(d => helper.factorial(d)).sum))
        .take(50000)
        .filter(p => p._1 == p._2)
        .map(p => p._1)
        .sum
    println(sum)
}






