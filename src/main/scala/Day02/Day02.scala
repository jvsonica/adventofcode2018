package Day02
import scala.io.Source


class Day02 {
  println("day02")

  def star1(): Int = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day02/input02.txt")

    // Counters
    var exactlyTwo: Int = 0
    var exactlyThree: Int = 0

    // Loop through file lines
    for (line <- input.getLines()) {

      // Create occurences map by gruping chars and getting the length of each group
      var occurrencesMap = line.groupBy(_.toChar).map { p => (p._1, p._2.length) }

      // If the values have any occurence with value 2, update the counter
      if (occurrencesMap.valuesIterator.toList contains 2) {
        exactlyTwo = exactlyTwo + 1
      }

      // If the values have any occurence with value 3, update the counter
      if (occurrencesMap.valuesIterator.toList contains 3) {
        exactlyThree = exactlyThree + 1
      }

    }

    // println(f"Exactly two occurred $exactlyTwo%d times and exactly three occurred $exactlyThree%d times.")
    return exactlyTwo * exactlyThree
  }

  def star2(): String = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day02/input02.txt")

    // Store in array
    val lines : Array[String] = (for (line <- input.getLines()) yield line).toArray

    // Loop through file lines
    for(i <- 0 to lines.length - 1) {
      for(j <- i + 1 to lines.length - 1) {
        // Filter first string to have only elements that match the second string in the same position
        var matched = lines(i)
          .zipWithIndex
          .filter{ case (c, index) => c == lines(j).charAt(index)}
          .map(_._1)

        if (matched.length == lines(i).length - 1) {
          return matched mkString ""
        }
      }
    }
    return "Not found"
  }
}
