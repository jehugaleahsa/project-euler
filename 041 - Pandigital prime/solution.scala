package euler

class PrimeBuilder {
    private[this] val primeList = scala.collection.mutable.ArrayBuffer[Long]()
    private[this] val primes = scala.collection.mutable.Set[Long]()
    private[this] var maxN = 1
    
    private[this] def isPrime(value: Long): Boolean = primeList.forall(value % _ != 0)
    
    private[this] def setPrimes(n: Int): Unit = {
    
        val sqrt = scala.math.sqrt(n).toInt + 1
    
        def sieve(value: Long) : Unit = {
            if (value <= sqrt) {
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
        isPrime(n.toLong)
    }
    
}

object Solution extends App {

    def getPermutations[T](values: IndexedSeq[T]): Seq[IndexedSeq[T]] = {
        if (values.length == 0) {
            Seq.fill(1)(IndexedSeq.empty[T])
        } else {
            for (i <- 0 until values.length; result <- {
                val leading = values.take(i)
                val current = values.drop(i).take(1)
                val trailing = values.drop(i + 1)
                val child = leading ++ trailing
                val results = for (result <- getPermutations(child)) yield {
                    current ++ result
                }
                results
            }) yield result
        }        
    }
    
    def toInt(digits: IndexedSeq[Int]): Int = {
        def impl(index: Int, soFar: Int): Int = {
            if (index == digits.length) soFar
            else impl(index + 1, soFar * 10 + digits(index))
        }
        impl(0, 0)
    }
    
    val primeBuilder = new PrimeBuilder
    val max = (9 to 1 by -1)
        .map(d => (1 to d).toIndexedSeq)
        .flatMap(d => getPermutations(d))
        .map(p => toInt(p))
        .filter(n => primeBuilder.isPrime(n))
        .max
    println(max)
}