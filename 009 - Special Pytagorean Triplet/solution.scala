package euler

object Solution extends App {

    var max = 1000
    val triplets = for {
        a <- 1 to max;
        b <- a to max;
        c <- b to max if (a * a) + (b * b) == (c * c)
    } yield (a, b, c)
    
    val theSpecial = triplets.filter(t => t._1 + t._2 + t._3 == max).head
    val product = theSpecial._1 * theSpecial._2 * theSpecial._3
    Console.println(product);
}