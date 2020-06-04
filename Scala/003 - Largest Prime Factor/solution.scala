package euler

object Solution extends App {
    
    def getPrimes(max: Long): Seq[Long] = {
    
        val sqrt = Math.sqrt(max)
            
        def isPrime(value: Long, primes: List[Long]) : Boolean 
            = primes.forall(value % _ != 0)
    
        def sieve(value: Long, primes: List[Long]) : List[Long] =
            if (value > sqrt) primes
            else if (isPrime(value, primes)) sieve(value + 1, value :: primes)
            else sieve(value + 1, primes)
        
        sieve(2, Nil);      
    }
    
    val max = 600851475143L
    val largestFactor = getPrimes(max).filter(max % _ == 0).head
    println(largestFactor)
}