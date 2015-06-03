package euler

class PrimeBuilder {
    private[this] val primes = scala.collection.mutable.Set[Long]()
    private[this] var maxN = 1
    
    private[this] def setPrimes(n: Int): Unit = {
            
        def isPrime(value: Long) : Boolean = primes.forall(value % _ != 0)
    
        def sieve(value: Long) : Unit = {
            if (value <= n) {
                if (isPrime(value)) {
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

    def getQuadraticResult(n: Int, a: Int, b: Int): Int = {
        n * n + a * n + b
    }
    
    val builder = new PrimeBuilder()    
    
    val results = for (a <- -999 to 999; b <- -999 to 999) yield {
        val primeLength = Stream.from(0)
            .map(n => getQuadraticResult(n, a, b))
            .takeWhile(q => builder.isPrime(q))
            .length
        (a, b, primeLength)
    }
    val result = results.maxBy(p => p._3)
    val maxProduct = result._1 * result._2
    println(maxProduct)
}