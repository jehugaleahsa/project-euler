package euler

object Solution extends App {

    import scala.annotation.tailrec
    
    def fibonacci(first: Int, second: Int, max: Int): List[Int] = {
    
        @tailrec
        def fib(first: Int, second: Int, resultBuilder: List[Int] => List[Int]) : List[Int] = (first + second) match {
            case sum if (sum >= max) => resultBuilder(Nil)
            case sum => fib(second, sum, r => resultBuilder(sum :: r))
        }
        
        fib(first, second, r => first :: second :: r)
        
    }
    
    val sum = fibonacci(1, 2, 4000000).filter(_ % 2 == 0).sum
    Console.println(sum);
    
}