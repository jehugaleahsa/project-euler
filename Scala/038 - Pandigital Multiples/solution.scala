package euler

object Solution extends App {

    def isPandigital(value: String): Boolean = {
        val digits = Set.empty ++ value
        (digits &~ Set('0')).size == 9
    }

    val max = (1 to 9327)
        .flatMap(n => 
            Stream.from(2)
                .map(x => (1 to x))
                .map(p => p.foldLeft(StringBuilder.newBuilder)((s, q) => s.append(q * n)))
                .takeWhile(p => p.length <= 9)
                .filter(p => p.length == 9)
                .map(p => p.toString)
                .filter(p => isPandigital(p))
        )
        .max
    println(max)
}