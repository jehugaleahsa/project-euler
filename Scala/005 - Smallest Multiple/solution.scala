package euler

object Solution extends App {

    var smallest = Stream.from(1)
        .filter(j => (20 to 2 by -1).forall(i => j % i == 0))
        .head;
    Console.println(smallest)
}