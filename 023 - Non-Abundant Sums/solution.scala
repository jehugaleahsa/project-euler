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
	
	def hasPair(value: Int, values: IndexedSeq[Int]): Boolean = {
	
		def lowerBound(front: Int, back: Int): Int = {
			val count = back - front
			if (count == 0) front
			else {
				val half = count / 2
				val middle = front + half
				val next = values(middle)
				if (next < value) lowerBound(middle + 1, back)
				else lowerBound(front, front + half)
			}
		}
		
		def hasPairImpl(front: Int, back: Int): Boolean = {
			val left = values(front)
			val right = values(back)
			val sum = left + right
			if (sum == value) true
			else if (front == back) false
			else if (right >= value) hasPairImpl(front, back - 1)
			else if (sum < value) hasPairImpl(front + 1, back)
			else hasPairImpl(front, back - 1)
		}
		
		val index = scala.math.min(lowerBound(0, values.length), values.length - 1)
		hasPairImpl(0, index)
	}

	val helper = new FactorHelper
	
	val abundantNumbers = (0 until 28123)
		.map(i => (i, helper.getProperFactors(i)))
		.map(p => (p._1, p._2.sum))
		.filter(p => p._2 > p._1)
		.map(p => p._1)
		.toIndexedSeq
	
	val sum = (1 to 28123)
		.filter(x => !hasPair(x, abundantNumbers))
		.map(x => x.toLong)
		.sum
	println(sum)
}
