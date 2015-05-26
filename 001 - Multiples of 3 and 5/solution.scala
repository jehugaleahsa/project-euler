package euler

object Solution extends App {
    
    def multiples(max: Int): Seq[Int] =
        for (i <- 0 until max if i % 3 == 0 || i % 5 == 0) yield i;
    
    val sum = multiples(1000).sum;
    Console.println(sum);
    
}