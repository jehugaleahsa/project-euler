package euler

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
    
    def toLong(digits: IndexedSeq[Int], start: Int, end: Int): Long = {
        def impl(index: Int, soFar: Long): Long = {
            if (index == end) soFar
            else impl(index + 1, soFar * 10 + digits(index))
        }
        impl(start, 0)
    }
    
    def isMatch(digits: IndexedSeq[Int]): Boolean = {
        val expected = IndexedSeq(2, 3, 5, 7, 11, 13, 17)
        (1 to 7)
            .map(n => toLong(digits, n, n + 3))
            .zipWithIndex
            .forall({ case (n, i) => n % expected(i) == 0})
    }

    val digits = (0 to 9).toIndexedSeq
    val permutations = getPermutations(digits).filter(_.head != 0)
    val matches = permutations
        .filter(p => isMatch(p))
        .map(p => toLong(p, 0, p.length))
    val sum = matches.sum
    println(sum)
}