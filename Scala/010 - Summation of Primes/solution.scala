package euler

object Solution extends App {

    def getPrimes(): Stream[Long] = {
    
        import scala.collection.mutable.ArrayBuffer
            
        def isPrime(value: Long, primes: ArrayBuffer[Long]) : Boolean 
            = primes.forall(value % _ != 0)
    
        def sieve(value: Long, primes: ArrayBuffer[Long]) : Stream[Long] = {
            val next = value + 1
            if (isPrime(value, primes)) {
                primes.append(value)
                Stream.cons(value, sieve(next, primes))
            } else {
                sieve(next, primes)
            }
        }
        
        sieve(2, new ArrayBuffer[Long]);      
    }
    
    val sum = getPrimes
        .zipWithIndex
        .map(p => {
            if (p._2 % 100 == 0) Console.println(f"${p._2}: ${p._1}")
            p._1
        })
        .takeWhile(_ < 2000000)
        .sum
    Console.println(sum)
}