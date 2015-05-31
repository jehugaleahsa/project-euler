package euler

object Solution extends App {

	def isLeapYear(year: Int): Boolean = {
		if (year % 100 == 0) {
			year % 400 == 0
		} else {
			year % 4 == 0
		}
	}
	
	def getNumberOfDays(year: Int, month: Int): Int = month match {
		case 1 => 31
		case 2 if (isLeapYear(year)) => 29
		case 2 => 28
		case 3 => 31
		case 4 => 30
		case 5 => 31
		case 6 => 30
		case 7 => 31
		case 8 => 31
		case 9 => 30
		case 10 => 31
		case 11 => 30
		case 12 => 31
	}
	
	def getNumberOfDays(year: Int): Int = {
		(1 to 12).map(getNumberOfDays(year, _)).sum
	}
	
	def getNumberOfDaysForEachMonth(startYear: Int, endYear: Int): Traversable[Int] = {
		for (year <- startYear to endYear; month <- 1 to 12) yield getNumberOfDays(year, month)
	}

	val sunday = 0
	val monday = 1
	val daysPerWeek = 7
	
	@scala.annotation.tailrec
	def getOccurrences(
		dayOfWeek: Int, 
		daysInMonths: List[Int], 
		builder: Int => Int): Int = daysInMonths match {
			case Nil => builder(0)
			case daysInMonth :: remaining => {
				val nextDayOfWeek = (dayOfWeek + daysInMonth) % daysPerWeek
				val isOccurrence = if (nextDayOfWeek == sunday) 1 else 0
				getOccurrences(nextDayOfWeek, remaining, c => builder(c + isOccurrence))
			}
	}

	val firstDay = monday + getNumberOfDays(1900) % daysPerWeek
	var	dayOfWeek = firstDay
	val daysInMonths = getNumberOfDaysForEachMonth(1901, 2000).toList
	var occurrences = getOccurrences(firstDay, daysInMonths, c => c)
	println(occurrences)
}