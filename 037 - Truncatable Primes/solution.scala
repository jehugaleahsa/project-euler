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

    val primeBuilder = new PrimeBuilder
    
    def allPrimesRight(value: Int): Boolean = {
        def impl(value: String): Boolean = {
            if (value.length == 0) true
            else if (!primeBuilder.isPrime(value.toInt)) false
            else impl(value.substring(1))
        }
        impl(value.toString)
    }
    
    def allPrimesLeft(value: Int): Boolean = {
        def impl(value: String): Boolean = {
            if (value.length == 0) true
            else if (!primeBuilder.isPrime(value.toInt)) false
            else impl(value.substring(0, value.length - 1))
        }
        impl(value.toString)
    }
    
    val sum = Stream.from(11)
        .filter(n => primeBuilder.isPrime(n))
        .filter(n => allPrimesRight(n))
        .filter(n => allPrimesLeft(n))
        .take(11)
        .sum
    println(sum)
}






