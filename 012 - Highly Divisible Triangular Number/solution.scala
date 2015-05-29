package euler

class Memoize[-T, +R](f: T => R) extends (T => R) {
	import scala.collection.mutable
	private[this] val results = mutable.Map.empty[T, R]
	
	def apply(x : T): R = {
		if (results.contains(x)) {
			results(x)
		} else {
			val result = f(x)
			results += ((x, result))
			result
		}
	}
}

object Memoize {
	def apply[T, R](f : T => R) = new Memoize(f)
}

object Solution extends App {

	import scala.annotation.tailrec
	
	def getTriangleNumber(max: Long) : Long = {
	
		@tailrec
		def add(next: Long, adder: Long => Long) : Long = next match {
			case 0 => adder(0)
			case _ => add(next - 1, sum => sum + adder(next))
		}
		
		add(max, sum => sum)
	}
	
	def getPrimes(max: Long): Stream[Long] = {
    
        import scala.collection.mutable.ArrayBuffer
            
        def isPrime(value: Long, primes: ArrayBuffer[Long]) : Boolean 
            = primes.forall(value % _ != 0)
    
        def sieve(value: Long, primes: ArrayBuffer[Long]) : Stream[Long] = {
            val next = value + 1
			if (value > max) {
				Stream.empty[Long]
            } else if (isPrime(value, primes)) {
                primes.append(value)
                Stream.cons(value, sieve(next, primes))
            } else {
                sieve(next, primes)
            }
        }
        
        sieve(2, new ArrayBuffer[Long]);      
    }
	
	def getFactors(value: Long) : Int = {
	
		def getMaxFactor(value: Long) : Long = value match {
			case 1 => 1
			case _ => getPrimes(value).filter(value % _ == 0).head
		}
		
		def impl(value: Long) : Stream[Long] = getMaxFactor(value) match {
			case 1 => Stream.empty[Long]
			case factor => {
				val quotient = value / factor
				Stream.cons(factor, impl(quotient))
			}
		}
		
		impl(value)
			.groupBy(x => x) // group primes together (exponents)
			.values
			.map(v => v.length + 1) // add one to the exponents
			.product // multiply the exponents
	}
	
	val getFast = Memoize(getTriangleNumber)
	val factorCount = 500
	val number = Stream.iterate(1L)(_ + 1)
		.map(getFast)
		.map(x => (x, getFactors(x)))
		.dropWhile(p => p._2 < factorCount)
		.map(p => p._1)
		.head
	println(number)
}