package euler

object Solution extends App {

	val tree = IndexedSeq(
		IndexedSeq(75),
		IndexedSeq(95, 64),
		IndexedSeq(17, 47, 82),
		IndexedSeq(18, 35, 87, 10),
		IndexedSeq(20,  4, 82, 47, 65),
		IndexedSeq(19,  1, 23, 75,  3, 34),
		IndexedSeq(88,  2, 77, 73,  7, 63, 67),
		IndexedSeq(99, 65,  4, 28,  6, 16, 70, 92),
		IndexedSeq(41, 41, 26, 56, 83, 40, 80, 70, 33),
		IndexedSeq(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
		IndexedSeq(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
		IndexedSeq(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
		IndexedSeq(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
		IndexedSeq(63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
		IndexedSeq( 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23)
	)
	
	def getMax(depth: Int, position: Int) : Int = {
		if (depth >= tree.length) 0
		else {
			val left = getMax(depth + 1, position)
			val right = getMax(depth + 1, position + 1)
			val max = scala.math.max(left, right)
			val value = tree(depth)(position)
			value + max
		}
	}
	
	val max = getMax(0, 0)
	println(max)
}