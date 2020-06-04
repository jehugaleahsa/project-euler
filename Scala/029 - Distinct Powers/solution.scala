package euler

object Solution extends App {

    val terms = for (a <- 2 to 100; b <- 2 to 100) yield BigInt(a).pow(b)
    val termSet = terms.toSet.toList
    println(termSet.length)
}