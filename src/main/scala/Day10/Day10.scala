package Day10

import scala.io.Source

class Day10 {

  case class Point(x : Int, y : Int, vx: Int, vy: Int, initialX: Int, initialY: Int) {
    def tickTime(): Point = copy(x = x + vx, y = y + vy)
  }

  def printSky(points : Array[Point]): Unit = {
    // Get size of print
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min

    // Matrix dimensions
    val xLength = Math.abs(points.map(_.x).max - points.map(_.x).min) + 1
    val yLength = Math.abs(points.map(_.y).max - points.map(_.y).min) + 1

    var matrix = Array.fill(yLength, xLength)('.')

    for (point <- points) {
      if (point.x >= 0 && point.y >= 0) {
        matrix(point.y - minY)(point.x - minX) = '#'
      }
    }

    for (i <- matrix.indices) { // Traversing elements by using loop
      for (j <- matrix(i).indices) {
        print(" " + matrix(i)(j))
      }
      println()
    }
  }

  def star1And2() : Int = {
    val input = Source.fromFile("src/main/scala/Day10/input10.txt")

    // Parsing helper
    val pattern = """^position=<(\s?\-?\d*),\s(\s?\-?\d*)>\svelocity=<(\s?\-?\d*),\s(\s?\-?\d*)>$""".r

    // Array of parsed points
    var points : Array[Point] = Array[Point]()

    for (line <- input.getLines()) {
      val pattern(x, y, vx, vy) = line
      points :+= Point(x.trim().toInt, y.trim().toInt, vx.trim().toInt, vy.trim().toInt, x.trim().toInt, y.trim().toInt)
    }

    var seconds = 0

    // Get size of print
    var xLength = Math.abs(points.map(_.x).max - points.map(_.x).min)
    var yLength = Math.abs(points.map(_.y).max - points.map(_.y).min)

    while (yLength >= 10) {

      for (i <- points.indices) {
        points(i) = points(i).tickTime()
      }

      seconds = seconds + 1

      // Get size of print
      xLength = Math.abs(points.map(_.x).max - points.map(_.x).min)
      yLength = Math.abs(points.map(_.y).max - points.map(_.y).min)
    }

    printSky(points)

    seconds
  }
}
