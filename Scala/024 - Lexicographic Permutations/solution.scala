package euler

object Solution extends App {

    import scala.math.Ordering.Implicits._

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
    
    val numbers = IndexedSeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val permutations = getPermutations(numbers)
    permutations.sorted
        .drop(999999)
        .take(1)
        .map(l => l.mkString("[", ",", "]"))
        .foreach(println)
}
