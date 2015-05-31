package euler

class FactorHelper {

	private[this] val map = scala.collection.mutable.Map[Int, List[Int]]()

	private[this] def getProperFactors(value: Int, next: Int): List[Int] = {
		if (next == 0) Nil
		else if (value % next == 0) next :: getProperFactors(value, next - 1)
		else getProperFactors(value, next - 1)
	}
	
	def getProperFactors(value: Int): List[Int] = {
		val first = value / 2
		val factors = getProperFactors(value, first)
		//map(value) = factors
		factors
	}

}

object Solution extends App {
	
	val helper = new FactorHelper
	val sum = (2 to 10000)
		.map(x => (x, helper.getProperFactors(x)))
		.map(p => (p._1, p._2.sum))
		.filter(p => p._1 != p._2)
		.map(p => (p._1, p._2, helper.getProperFactors(p._2)))
		.map(p => (p._1, p._2, p._3.sum))
		.filter(p => p._1 == p._3)
		.map(p => p._1)
		.distinct
		.sum
	println(sum)
}
