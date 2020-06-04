package euler

class PrimeBuilder {
    private[this] val primeList = scala.collection.mutable.ArrayBuffer[Long]()
    private[this] val primes = scala.collection.mutable.Set[Long]()
    private[this] var maxN = 1
    
    private[this] def setPrimes(n: Int): Unit = {
            
        def isPrime(value: Long) : Boolean = primeList.forall(value % _ != 0)
    
        def sieve(value: Long) : Unit = {
            if (value <= n) {
                if (isPrime(value)) {
                    primeList.append(value)
                    primes.add(value)
                }
                sieve(value + 1)
            }
        }
        
        sieve(maxN + 1);    
    }
    
    def isPrime(n: Int): Boolean = {        
        if (n > maxN) {
            setPrimes(n)
            maxN = n
        }
        primes.contains(n)
    }
    
}

object Solution extends App {

    def getPermutations[T](values: IndexedSeq[T]): Seq[IndexedSeq[T]] = {
        for (i <- 0 until values.length) yield {
            val leading = values.take(i)
            val trailing = values.drop(i)
            trailing ++ leading
        }
    }
    
    def toInt(digits: IndexedSeq[Int]): Int = {    
        def impl(index: Int, soFar: Int): Int =
            if (index == digits.length) soFar
            else impl(index + 1, soFar * 10 + digits(index))
        impl(0, 0)    
    }
    
    def getDigits(value: Int): IndexedSeq[Int] = {
        def impl(value: Int): IndexedSeq[Int] =
            if (value == 0) IndexedSeq.empty[Int]
            else (value % 10).toInt +: impl(value / 10)
        impl(value).reverse
	}
    
    val max = 1000000
    val primeBuilder = new PrimeBuilder
    
    val count = (2 to max)
        .filter(n => primeBuilder.isPrime(n))
        .map(n => (n, getPermutations(getDigits(n))))
        .map(p => (p._1, p._2.map(c => toInt(c))))
        .filter(p => p._2.forall(c => primeBuilder.isPrime(c)))
        .map(p => p._1)
        .length
    println(count)
}






