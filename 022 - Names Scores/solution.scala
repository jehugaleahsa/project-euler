package euler

object Solution extends App {

	def getValue(letter: Char): Int = {
		val minValue = 'A'.toInt
		val currentValue = letter.toInt
		currentValue - minValue + 1
	}
	
	def getValue(name: String): Int = {
		name.map(getValue(_)).sum
	}
	
	val source = scala.io.Source.fromFile("p022_names.txt")
	val lines = try source.getLines.toList finally source.close()
	val names = lines
		.flatMap(l => l.split(",").map(_.stripPrefix("\"").stripSuffix("\"")))
		.sorted
	val sum = names
		.zipWithIndex
		.map(p => (p._1, p._2 + 1))
		.map(p => (p._1, getValue(p._1), p._2))
		.map(p => (p._1, p._2 * p._3))
		.map(p => p._2.toLong)
		.sum
	println(sum)
}
