package euler

object Solution extends App {

	import scala.collection.mutable.Map
	
	val resultLookup = Map[(Long, Long), Long]()

	def tryPath(x: Long, y: Long, maxX: Long, maxY: Long) : Long = {
		if (resultLookup.contains((x, y))) resultLookup((x, y))
		else if (x == maxX && y == maxY) 1L // we made it to the end, this path succeeded
		else if (x > maxX) 0L // this path goes beyond the right barrier, so it fails
		else if (y > maxY) 0L // this path goes beyond the bottom barrier, so it fails
		else {
			val right = tryPath(x + 1, y, maxX, maxY)
			val down = tryPath(x, y + 1, maxX, maxY)
			val sum = right + down
			resultLookup((x, y)) = sum
			sum
		}
	}
	
	val maxX = 20
	val maxY = 20
	val totalPaths = tryPath(0, 0, maxX, maxY)
	println(totalPaths)
}