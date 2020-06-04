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
	val words = lines.flatMap(l => l.split(",").map(_.stripPrefix("\"").stripSuffix("\"")))
}
