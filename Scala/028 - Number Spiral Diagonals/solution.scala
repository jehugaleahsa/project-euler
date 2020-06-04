package euler

object Solution extends App {

    import scala.collection.mutable.IndexedSeq

    def getLength(level: Int): Int = level match {
        case 0 => 1
        case _ => getLength(level - 1) + 2
    }

    def buildSpiral(length: Int): IndexedSeq[IndexedSeq[Int]] = {
        val spiral = IndexedSeq.fill(length)(IndexedSeq.fill(length)(0))    
        var (x, y) = (length / 2, length / 2)
        spiral(x)(y) = 1
        var next = 2
        var level = 0
        while (y + 1 < length) {        
            level = level + 1
        
            // Go to the right by 1
            y = y + 1
            spiral(x)(y) = next
            next = next + 1
            
            // Go down
            var fullSpan = getLength(level)
            val downSpan = fullSpan - 1
            for (i <- 0 until downSpan - 1) {
                spiral(x + i + 1)(y) = next
                next = next + 1
            }
            x = x + downSpan - 1
            
            // Go left
            for (i <- 0 until fullSpan - 1) {
                spiral(x)(y - i - 1) = next
                next = next + 1
            }
            y = y - fullSpan + 1
            
            // Go up
            for (i <- 0 until fullSpan - 1) {
                spiral(x - i - 1)(y) = next
                next = next + 1
            }
            x = x - fullSpan + 1
            
            // Go right
            for (i <- 0 until fullSpan - 1) {
                spiral(x)(y + i + 1) = next
                next = next + 1
            }
            y = y + fullSpan - 1
        }
        spiral
    }

    val length = 1001
    val spiral = buildSpiral(length)
    
    val sums = for (a <- 0 until length) yield {
        spiral(a)(a) + spiral(length - a - 1)(a)
    }
    val sum = sums.sum - 1 // don't count the center twice
    println(sum)
}