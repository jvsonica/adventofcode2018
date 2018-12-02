package Day01
import scala.io.Source

class Day01 {
  println("day01");

  def star1 (initialCurrent:Int) : Int  = {
    // Initialize result with initial current
    var result : Int = initialCurrent

    // Read input file
    val input = Source.fromFile("src/main/scala/Day01/input01.txt")

    // Loop through file lines
    for (line <- input.getLines()) {

      // First char in each line is the operation
      var operation : Char = line.charAt(0)

      // Following characters in each line are the value
      var value : Int = line.substring(1).toInt

      // Change result according to the current line
      operation match {
        case '+'  => result = result + value
        case '-'  => result = result - value
        case whoa  => println("Unexpected case: " + whoa.toString)
      }
    }
    input.close()

    return result
  }

  def start2 (initialCurrent: Int) : Int = {
    // Initialize variable that will keep track of current in each iteration
    var current : Int = initialCurrent

    // Hashmap that will save reached values
    var cachedResults = Map[Int, Boolean]((0 -> true))

    // Read input file
    val input = Source.fromFile("src/main/scala/Day01/input01.txt")

    // Store in array
    val lines = (for (line <- input.getLines()) yield line).toSeq

    // Loop through file lines the amount of times needed
    while (true) {

      // Loop through lines
      for (line <- lines) {

        // First char in each line is the operation
        var operation : Char = line.charAt(0)

        // Following characters in each line are the value
        var value : Int = line.substring(1).toInt

        // Change result according to the current line
        operation match {
          case '+'  => current = current + value
          case '-'  => current = current - value
          case whoa  => println("Unexpected case: " + whoa.toString)
        }

        if (cachedResults.get(current).isDefined) {
          input.close()
          return current
        }

        cachedResults = cachedResults.updated(current, true)
      }
    }
    throw new Exception("End of loop")  }
}
