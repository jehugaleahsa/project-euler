package euler

object Solution extends App {

	val ones = IndexedSeq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
	val teens = IndexedSeq(
		"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
		"sixteen", "seventeen", "eighteen", "nineteen"
	)
	val tens = IndexedSeq(
		null, null, "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
	)
	
	def toOnes(values: List[Int]) : List[String] = values match {
		case head :: Nil if (head == 0) => Nil
		case head :: Nil => ones(head) :: Nil
		case _ => throw new Exception()
	}
	
	def toTens(values: List[Int]) : List[String] = values match {
		case head :: tail if (head == 0) => toOnes(tail)
		case head1 :: head2 :: Nil if (head1 == 1) => teens(head2) :: Nil
		case head1 :: head2 :: Nil if (head2 == 0) => tens(head1) :: Nil
		case head :: tail => tens(head) :: toOnes(tail)
	}

	def toHundreds(values: List[Int]) : List[String] = values match {
		case head :: tail if (head == 0) => toTens(tail)
		case head :: tail => toTens(tail) match {
			case Nil => ones(head) :: "hundred" :: Nil
			case next => ones(head) :: "hundred" :: "and" :: next
		}
	}

	def toThousands(values: List[Int]) : List[String] =
		"one" :: "thousand" :: toHundreds(values.tail)
	
	val formatters = IndexedSeq(toOnes _, toOnes _, toTens _, toHundreds _, toThousands _)
	
	def toDigits(value: Int): List[Int] = {
		if (value == 0) Nil
		else value % 10 :: toDigits(value / 10)
	}
	
	val total = Stream.from(1)
		.take(1000)
		.map(x => toDigits(x).reverse)
		.flatMap(l => formatters(l.length)(l))
		.map(l => l.length)
		.sum
	println(total)
}