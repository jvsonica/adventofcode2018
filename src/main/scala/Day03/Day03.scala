package Day03
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class Day03 {
  println("day03")

  def star1(): Int = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day03/input03.txt")

    // Fabric matrix
    val matrix = ArrayBuffer.fill(1000, 1000)(0)

    // Regex to match each line
    val pattern = "^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

    // Loop through file lines
    for (line <- input.getLines()) {
      var pattern(id, x, y, width, height) = line

      // Fill rectangles in matrix
      for (i <- x.toInt to x.toInt + width.toInt - 1) {
        for (j <- y.toInt to y.toInt + height.toInt - 1) {
          matrix(i.toInt)(j.toInt) = matrix(i.toInt)(j.toInt) + 1
        }
      }
     }

    // Count cells with more than 2 occurences
    var count : Int = 0
    for (i <- 0 to matrix.length - 1) {
      for (j <- 0 to matrix(i).length - 1) {
        if (matrix(i)(j) >= 2) {
          count = count + 1
        }
      }
    }

    return count
  }

  def star2(): Int = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day03/input03.txt")

    // Store in array
    val lines : Array[String] = (for (line <- input.getLines()) yield line).toArray

    // Save result
    var claimId : Int = 0

    // Fabric matrix
    val matrix = ArrayBuffer.fill(1000, 1000)(0)

    // Regex to match each line
    val pattern = "^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

    // Loop through file lines
    for(line <- lines) {
      val pattern(id, x, y, width, height) = line
      // Fill rectangles in matrix
      for (i <- x.toInt to x.toInt + width.toInt - 1) {
        for (j <- y.toInt to y.toInt + height.toInt - 1) {
          matrix(i.toInt)(j.toInt) = matrix(i.toInt)(j.toInt) + 1
        }
      }
    }

    // Loop through file lines again
    for (line <- lines) {
      val pattern(id, x, y, width, height) = line
      var allOnes = true

      // Fill rectangles in matrix
      for (i <- x.toInt to x.toInt + width.toInt - 1) {
        for (j <- y.toInt to y.toInt + height.toInt - 1) {
          if (matrix(i.toInt)(j.toInt) != 1) {
            allOnes = false
          }
        }
      }

      if (allOnes) {
        claimId = id.toInt
      }
    }

    return claimId
  }

  def show(matrix : ArrayBuffer[ArrayBuffer[Int]]) {
    for (i <- 0 to matrix.length - 1) { // Traversing elements by using loop
      for (j <- 0 to matrix(i).length - 1) {
        print(" " + matrix(i)(j))
      }
      println()
    }
  }
}
