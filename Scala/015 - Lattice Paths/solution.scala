package euler

object Solution extends App {
	
    val maxX = 20
	val maxY = 20
	val resultLookup = scala.collection.mutable.IndexedSeq.fill[Long](maxX, maxY)(0L)
    
    def cache(x: Int, y: Int, f: (Int, Int) => Long): Long = {
        val result = f(x, y)
        if (x < maxX && y < maxY) {
            resultLookup(x)(y) = result
        }
        result
    }

	def tryPath(x: Int, y: Int): Long = {
		if (x == maxX && y == maxY) 1L // we made it to the end, this path succeeded
        else if (x == maxX) cache(x, y + 1, tryPath _)
        else if (y == maxY) cache(x + 1, y, tryPath _)
        else if (resultLookup(x)(y) != 0L) resultLookup(x)(y)
		else cache(x + 1, y, tryPath _) + cache(x, y + 1, tryPath _)
	}

	val totalPaths = tryPath(0, 0)
	println(totalPaths)
}