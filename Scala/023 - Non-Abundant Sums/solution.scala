package euler

class FactorHelper {

	private[this] def getProperFactors(value: Int, next: Int): List[Int] = {
		if (next == 0) Nil
		else if (value % next == 0) next :: getProperFactors(value, next - 1)
		else getProperFactors(value, next - 1)
	}
	
	def getProperFactors(value: Int): List[Int] = {
		val first = value / 2
		val factors = getProperFactors(value, first)
		factors
	}

}

object Solution extends App {
    
    val helper = new FactorHelper
	val abundantNumbers = (0 until 28123)
		.map(i => (i, helper.getProperFactors(i)))
		.map(p => (p._1, p._2.sum))
		.filter(p => p._2 > p._1)
		.map(p => p._1)
		.toIndexedSeq
        
    val abundantSums = (for {
        i <- 0 until abundantNumbers.length; 
        j <- i until abundantNumbers.length
    } yield {
        val left = abundantNumbers(i);
        val right = abundantNumbers(j)
        left + right
    }).toSet
	
	val sum = (1 to 28123)
		.filter(x => !abundantSums.contains(x))
		.map(x => x.toLong)
		.sum
        
	println(sum)
}
