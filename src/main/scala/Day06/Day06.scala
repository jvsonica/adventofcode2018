package Day06
import scala.io.Source


class Day06 {
  println("day06")

  def manhattanDistance(x1: Int, y1 : Int, x2: Int, y2 : Int ) : Int = {
    math.abs(x1 - x2) + math.abs(y1 - y2)
  }

  def star1() : Int = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day06/input06.txt")

    // Input locations
    var locations : Map[Int, (Int, Int)] = Map[Int, (Int, Int)]()

    // Regex to read values in each line
    val pattern = """(\d*),\s(\d*)""".r

    // Read input into locations
    for ((line, index) <- input.getLines().zipWithIndex) {
      val pattern(x, y) = line
      locations += index -> (x.toInt, y.toInt)
    }

    // Get size of locations
    val xLimit = locations.values.map(_._1).max + 2
    val yLimit = locations.values.map(_._2).max + 2

    // Generate minimum matrix
    var matrix = Array.fill(xLimit, yLimit)(-1)

    // Filling matrix with known entries
    for ((loc, index) <- locations.zipWithIndex) {
      matrix(loc._2._1)(loc._2._2) = loc._1
    }

    // Filling in the matrix with the closest location
    val locationIndices = locations.keys.toArray
    for (x <- 0 until xLimit; y <- 0 until yLimit) {

      // For each position of the matrix we calculate manhattanDistance for every known location
      val distances = locationIndices.map(i => {
        manhattanDistance(locations(i)._1, locations(i)._2, x, y)
      })

      // Check if there is more than one equal distance. in that case keep the "-1"
      val leastDistance = distances.min
      val indicesWithLeastDistance = distances.zipWithIndex.filter(d => d._1 == leastDistance)

      // Fill with the id of the location if a single location was found
      if (indicesWithLeastDistance.length == 1) {
        matrix(x)(y) = indicesWithLeastDistance(0)._2
      }
    }

    // Check the frame of the matrix to detect infinite groups, and, consequently the finite groups
    val firstRow = matrix(0)
    val lastRow = matrix(xLimit - 1)
    val firstColumn = matrix.map(_.head)
    val lastColumn = matrix.map(_.last)

    var borders = firstRow ++ lastRow ++ firstColumn ++ lastColumn
    var finiteGroups = locations.keys.toArray.filter(!borders.distinct.contains(_))

    // Count occurrences of finite groups in matrix
    val flattenedMatrixWithFiniteGroups = matrix.flatten.filter(finiteGroups.contains(_))
    val countPerGroup = flattenedMatrixWithFiniteGroups.groupBy(identity).map(tuple => tuple._2.length)

    countPerGroup.max
  }

  def star2() : Int = {
    // Read input file
    val input = Source.fromFile("src/main/scala/Day06/input06.txt")
    val minDistance = 10000

    // Input locations
    var locations : Map[Int, (Int, Int)] = Map[Int, (Int, Int)]()

    // Regex to read values in each line
    val pattern = """(\d*),\s(\d*)""".r

    // Read input into locations
    for ((line, index) <- input.getLines().zipWithIndex) {
      val pattern(x, y) = line
      locations += index -> (x.toInt, y.toInt)
    }

    // Get size of locations
    val xLimit = locations.values.map(_._1).max + 2
    val yLimit = locations.values.map(_._2).max + 2

    // Generate minimum matrix
    var matrix = Array.fill(xLimit, yLimit)(-1)

    // Filling matrix with known entries
    for ((loc, index) <- locations.zipWithIndex) {
      matrix(loc._2._1)(loc._2._2) = loc._1
    }

    // Marking locations whose sum distances to locations are within minDistance
    val locationIndices = locations.keys.toArray
    for (x <- 0 until xLimit; y <- 0 until yLimit) {

      // For each position of the matrix we calculate manhattanDistance for every known location
      val distances = locationIndices.map(i => {
        manhattanDistance(locations(i)._1, locations(i)._2, x, y)
      })

      // Mark that cell with a -2 if it within minDistance
      if (distances.sum < minDistance) {
        matrix(x)(y) = -2
      }
    }

    // Count cells of safe zone
    val safeZoneSize = matrix.flatten.count(_ == -2)

    safeZoneSize
  }

  def show(matrix : Array[Array[Int]]) {
    for (i <- matrix.indices) { // Traversing elements by using loop
      for (j <- matrix(i).indices) {
        print(s" ${matrix(i)(j)}".padTo(3, ' '))
      }
      println()
    }
  }
}
