package euler

object Solution extends App {

    val numbers = Stream.from(1).take(100)
    val sum = numbers.sum
    val squares = numbers.map(i => i * i)
    
    val difference = (sum * sum) - squares.sum
    Console.println(difference);
}