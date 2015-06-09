package euler

class Rational(numerator: Int, denominator: Int) extends Ordered[Rational] {
    private[this] val _numerator = if (denominator < 0) -numerator else numerator
    private[this] val _denominator = denominator.abs
    
    def numerator(): Int = _numerator
    def denominator(): Int = _denominator
    
    override def toString(): String = numerator + "/" + denominator
    
    override def equals(obj: Any): Boolean = obj match {
        case other: Rational => {
            val rThis = reduce
            val rOther = other.reduce
            rThis.numerator == rOther.numerator && rThis.denominator == rOther.denominator
        }
        case _ => false
    }
    
    override def hashCode(): Int = {
        (numerator, denominator).hashCode
    }
    
    def reduce(): Rational = {
        val common = Rational.gcd(numerator.abs, denominator.abs)
        val newNumerator = numerator / common
        val newDenominator = denominator / common
        new Rational(newNumerator, newDenominator)
    }
    
    override def compare(other: Rational): Int = 
        (numerator * other.denominator) - (other.numerator * denominator)
}

object Rational {
    def apply(numerator: Int, denominator: Int): Rational = 
        new Rational(numerator, denominator)
    
    private def gcd(first: Int, second: Int): Int = {
        if (second == 0) first
        else gcd(second, first % second)
    }
    
    import scala.language.implicitConversions
    implicit def intToRational(value: Int): Rational = Rational(value, 1)
    
    implicit object RationalFractional extends Fractional[Rational] {

        override def negate(value: Rational): Rational = {
            new Rational(-value.numerator, value.denominator)
        }
        
        override def plus(first: Rational, second: Rational): Rational = {
            val numerator = first.numerator * second.denominator + second.numerator * first.denominator
            val denominator = first.denominator * second.denominator
            new Rational(numerator, denominator)
        }
            
        override def minus(first: Rational, second: Rational): Rational = {
            val numerator = first.numerator * second.denominator - second.numerator * first.denominator
            val denominator = first.denominator * second.denominator
            new Rational(numerator, denominator)
        }
            
        override def times(first: Rational, second: Rational): Rational = {
            val numerator = first.numerator * second.numerator
            val denominator = first.denominator * second.denominator
            new Rational(numerator, denominator)
        }
        
        override def div(first: Rational, second: Rational): Rational = {
            val num = first.numerator * second.denominator
            val denom = first.denominator * second.numerator
            new Rational(num, denom) 
        }

        override def compare(first: Rational, second: Rational): Int = 
            (first.numerator * second.denominator) - (second.numerator * first.denominator)
        
        override def fromInt(value: Int): Rational = new Rational(value, 1)
        
        override def toDouble(value: Rational): Double = value.numerator.toDouble / value.denominator
        
        override def toFloat(value: Rational): Float = value.numerator.toFloat / value.denominator
        
        override def toInt(value: Rational): Int = value.numerator / value.denominator
        
        override def toLong(value: Rational): Long = value.numerator.toLong / value.denominator
    }
}

object Solution extends App {

    def getDigits(value: Int): List[Int] = {    
        def impl(value: Int): List[Int] = {
            if (value == 0) Nil
            else value % 10 :: getDigits(value / 10)
        }
        impl(value).reverse
    }

    val values = 10 to 99
    val fractions = for (n <- values; d <- values) yield Rational(n, d)
    val curiousFractions = fractions
        .filter(f => f < 1)
        .filter(f => f.numerator % 10 != 0 || f.denominator % 10 != 0)
        .filter(f => {
            val numeratorDigits = getDigits(f.numerator).toSet
            val denominatorDigits = getDigits(f.denominator).toSet
            val common = numeratorDigits & denominatorDigits
            if (common.isEmpty) false
            else {
                val cNumerator = numeratorDigits &~ common
                val cDenominator = denominatorDigits &~ common
                if (cNumerator.isEmpty || cDenominator.isEmpty) false
                else {
                    val badReduction = Rational(cNumerator.head, cDenominator.head)
                    val goodReduction = f.reduce
                    badReduction == goodReduction
                }
            }
        })
        
    curiousFractions.foreach(println)
    
    val product = curiousFractions.product.reduce
    println(product)
}






