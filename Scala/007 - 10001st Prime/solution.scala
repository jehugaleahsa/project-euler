package euler

object Solution extends App {

    def getPrimes(): Stream[Long] = {
            
        def isPrime(value: Long, primes: List[Long]) : Boolean 
            = primes.forall(value % _ != 0)
    
        def sieve(value: Long, primes: List[Long]) : Stream[Long] = {
            val next = value + 1
            if (isPrime(value, primes)) {
                Stream.cons(value, sieve(next, value :: primes))
            } else {
                sieve(next, primes)
            }
        }
        
        sieve(2, Nil);      
    }
    
    val prime = getPrimes.drop(10000).head
    Console.println(prime)
}