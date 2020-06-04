package euler

object Solution extends App {

    def getReciprocalCycle(n: Int): Stream[Int] = {
    
        val occurrances = scala.collection.mutable.Set[Int]()

        def getNext(numerator: Int): Stream[Int] = {
            if (numerator == 0 || occurrances.contains(numerator)) Stream.empty[Int]
            else {
                occurrances.add(numerator)
                val powd = numerator * 10
                Stream.cons(powd / n, getNext(powd % n))         
            }
        }
        
        getNext(1)
    }
    
    val longest = (2 until 1000)
        .map(x => (x, getReciprocalCycle(x).toList))
        .maxBy(p => p._2.length)
        ._1
    println(longest)
}