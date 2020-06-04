package euler

class Cacher {

	import scala.collection.mutable.Map
	import scala.annotation.tailrec
	
	private[this] val resultLookup = Map[Long, Long]()
	
	def getNext(value: Long) : Long = value match {
		case _ if (value % 2 == 0) => value / 2
		case _ => 3 * value  + 1
	}
	
	@tailrec
	private[this] def getSequenceImpl(value: Long, counter: Long => Long) : Long = {
		if (resultLookup.contains(value)) {
			counter(resultLookup(value))
		} else if (value == 1L) {
			resultLookup(value) = 1L
			counter(1L)
		} else {
			val next = getNext(value)
			getSequenceImpl(next, subCount => {
				resultLookup(next) = subCount
				counter(subCount + 1L)
			})
		}
	}
	
	def getSequence(value: Long) : Long = {
		getSequenceImpl(value, c => {
			resultLookup(value) = c
			c
		})
	}
}

object Solution extends App {

	val cacher = new Cacher
	
	val max = Stream.from(1)
		.take(1000000)
		.map(n => (n, cacher.getSequence(n)))
		.maxBy(p => p._2)
		._1
	println(max)
}