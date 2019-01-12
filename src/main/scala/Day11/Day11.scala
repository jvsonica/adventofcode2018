package Day11

import scala.collection.mutable.ArrayBuffer

class Day11 {
  println("day11")

  val GRID_SERIAL_NUMBER : Int = 7315

  case class Point(x : Int, y : Int) {
    def rackId : Int = x + 10
    def powerLevel : Int = (((this.rackId * y + GRID_SERIAL_NUMBER) * this.rackId) / 100 % 10) - 5
  }

  def star1() : String = {

    val matrix = Array.fill(300, 300)(0)

    // Fill matrix with power levels
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        val currentPoint = Point(i+1, j+1)
        matrix(i)(j) = currentPoint.powerLevel
      }
    }

    // Find max power level
    var maxFuelPower = 0
    var x = 0
    var y = 0

    for (i <- 0 to matrix.length - 3) {
      for (j <- 0 to matrix(i).length - 3) {

        val group = matrix(i)(j) + matrix(i+1)(j) + matrix(i+2)(j) +
          matrix(i)(j+1) + matrix(i+1)(j+1) + matrix(i+2)(j+1) +
          matrix(i)(j+2) + matrix(i+1)(j+2) + matrix(i+2)(j+2)

        if (group > maxFuelPower) {
          maxFuelPower = group
          x = i + 1
          y = j + 1
        }
      }
    }

    s"$x,$y"
  }

  def star2() : String = {

    val matrixSize = 300

    val matrix = Array.fill(matrixSize, matrixSize)(0)

    // Fill matrix with power levels
    for (i <- matrix.indices) {
      for (j <- matrix(i).indices) {
        val currentPoint = Point(i+1, j+1)
        matrix(i)(j) = currentPoint.powerLevel
      }
    }

    // Find max power level
    var maxFuelPower = 0
    var x = 0
    var y = 0
    var squareSize = 0

    // Loop thorugh every cell
    for (i <- matrix.indices) {
      for (j <- matrix.indices) {

        // Find the max size of each cell's square,
        // i.e. a cell at 200,0, can only have a max square of 100x100 (300-200=100)
        val maxSize = matrixSize - Array(i, j).max

        // Loop through all possible sizes
        for (currentSize <- 2 to maxSize) {

          // Calculate the square size with currentSize
          var groupSum = 0
          for (tempX <- i until i + currentSize) {
            for (tempY <- j until j + currentSize) {
              groupSum += matrix(tempX)(tempY)
            }
          }

          // Update max fuel group
          if (groupSum > maxFuelPower) {
            x = i + 1
            y = j + 1
            squareSize = currentSize
            maxFuelPower = groupSum
          }
        }
      }
    }

    s"$x,$y,$squareSize"
  }
}
