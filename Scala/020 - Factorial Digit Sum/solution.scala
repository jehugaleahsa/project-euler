package euler

object Solution extends App {

	val resultLookup = scala.collection.mutable.Map[BigInt, BigInt]()

	def factorial(value: BigInt): BigInt = {
		if (resultLookup.contains(value)) {
			resultLookup(value)
		} else if (value == 1) {
			resultLookup(value) = 1
			1
		} else {
			val result = value * factorial(value - 1)
			resultLookup(value) = result
			result
		}
	}
	
	def getDigits(value: BigInt): List[Int] = {
		if (value == 0) Nil
		else (value % 10).toInt :: getDigits(value / 10)
	}
	
	val result = factorial(BigInt(100))
	val digits = getDigits(result)
	val sum = digits.sum
	println(sum)
}