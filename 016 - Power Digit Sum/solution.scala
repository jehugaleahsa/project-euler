package euler

object Solution extends App {

	def getDigits(value: BigInt): List[BigInt] = {
		if (value == BigInt(0)) Nil
		else (value % 10) :: getDigits(value / 10)
	}

	val x = BigInt(2).pow(1000)
	val digits = getDigits(x)
	val sum = digits.sum
	println(sum)
}