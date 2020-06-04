package euler

object Solution extends App {

	def count(denominations: List[Int], previous: Int, amount: Int): Long = {
		if (amount < 0) 0L
		else if (amount == 0) 1L
		else {
			val subCounts = for (denomination <- denominations if denomination <= previous) yield {
				val remaining = amount - denomination
				val subCount = count(denominations, denomination, remaining)
				subCount
			}
			subCounts.sum
		}
	}
	
	val denominations = 1 :: 2 :: 5 :: 10 :: 20 :: 50 :: 100 :: 200 :: Nil
	val amount = 200
	val combinations = count(denominations, 200, amount)
	println(combinations)
}