package euler

class TriangleBuilder {
    private[this] val resultLookup = scala.collection.mutable.Set[Int]()
    private[this] var maxN = 0
    private[this] var maxValue = 0
    
    private[this] def setTriangles(search: Int): Boolean = {
        def getTerm(n: Int): Int = n * (n + 1) / 2
        def impl(n: Int): Boolean = {
            maxN = n
            maxValue = getTerm(maxN)
            resultLookup.add(maxValue)
            if (maxValue == search) true
            else if (maxValue > search) false
            else impl(n + 1)
        }
        impl(maxN + 1)
    }
    
    def isTriangular(value: Int): Boolean = {
        if (resultLookup.contains(value)) true
        else if (value < maxValue) false
        else setTriangles(value)
    }    
}

object Solution extends App {

    def toInt(letter: Char): Int = letter.toInt - 'A'.toInt + 1
    
    def toInt(word: String): Int = word.map(c => toInt(c)).sum

    val source = scala.io.Source.fromFile("p042_words.txt")
	val lines = try source.getLines.toList finally source.close()
	val words = lines.flatMap(l => l.split(",").map(_.stripPrefix("\"").stripSuffix("\"")))
    val builder = new TriangleBuilder
    val count = words.map(w => toInt(w)).filter(i => builder.isTriangular(i)).length
    println(count)
}