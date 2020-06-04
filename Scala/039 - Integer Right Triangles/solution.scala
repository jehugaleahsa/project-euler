package euler

object Solution extends App {

    def getTriples(max: Int): Seq[IndexedSeq[Int]] = {    
        def squared(value: Int): Int = value * value     
        def isPythagorean(x: Int, y: Int, h: Int): Boolean = squared(x) + squared(y) == squared(h)            
        val potentials = for (
            m <- 2 to max; 
            n <- 1 to m; 
            h = max - m - n;
            if isPythagorean(m, n, h)) yield (m, n, h)
        val triples = potentials
            .map(t => Set(t._1, t._2, t._3))
            .distinct
            .map(t => t.toIndexedSeq)
        triples
    }
    
    val triplePairs = (1 to 1000).map(p => (p, getTriples(p)))
    val maxSolutions = triplePairs.maxBy(p => p._2.length)._1
    println(maxSolutions)
}