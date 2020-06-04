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
    
    def toInt(digits: List[Int]): Int = {
    
        def impl(remaining: List[Int], soFar: Int): Int = remaining match {
            case Nil => soFar
            case head :: tail => impl(tail, soFar * 10 + head)
        }
        
        impl(digits, 0)        
    }
    
    val digits = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val permutations = getPermutations(digits)
    
    val combinations = for {
        permutation <- permutations;
        i <- 1 until permutation.length - 1;
        j <- i until permutation.length - 1
    } yield {
        val first = permutation.take(i)
        val second = permutation.drop(i).take(j - i + 1)
        val product = permutation.drop(j + 1)
        val firstNum = toInt(first.toList)
        val secondNum = toInt(second.toList)
        val productNum = toInt(product.toList)
        if (firstNum <= secondNum) (firstNum, secondNum, productNum)
        else (secondNum, firstNum, productNum)
    }
    val validCombinations = for (c <- combinations if (c._1 * c._2 == c._3)) yield c
    val sum = validCombinations.map(c => c._3.toLong).distinct.sum
    println(sum)
}