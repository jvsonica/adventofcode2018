package Day05
import scala.io.Source
import util.control.Breaks._


class Day05 {
  println("day05")

  def reactPolymers(polymerArr : Array[Char]) : Array[Char] = {
    var polymers = polymerArr

    // Break loop
    var operationPerformed : Boolean = true
    while(operationPerformed) {
      val initialLength = polymers.length

      // Using breakable as it seemed for intuitive, even though it's not the most "scala" way to do this
      breakable {
        for (i <- 0 until polymers.length - 1) {
          if (polymers(i) != polymers(i + 1) && polymers(i + 1).toUpper == polymers(i).toUpper) {
            // Take the polymers we don't want
            polymers = polymers.take(i) ++ polymers.drop(i+2)
            break
          }
        }
      }

      if (initialLength == polymers.length) {
        operationPerformed = false
      }
    }

    polymers
  }

  def star1() : Int = {
    // Read input
    var polymers = Source.fromFile("src/main/scala/Day05/input05.txt").toArray

    // React polymers
    val reactedPolymers = reactPolymers(polymers)

    reactedPolymers.length
  }

  def star2() : Int = {
    // Read input
    var polymers = Source.fromFile("src/main/scala/Day05/input05.txt").toArray

    // React polymers
    val reactedPolymers = reactPolymers(polymers)

    // Gather unique polymers
    val uniquePolymers = reactedPolymers.map(_.toUpper).distinct

    // Minimum value reached
    var minSequenceLength = 15000

    // Try it out without each polymer
    for (upperCasedPolymer <- uniquePolymers) {
      // Filtering both upper and lower case polymers
      val filteredPolymers = reactedPolymers.filter(c => c.toUpper != upperCasedPolymer)

      // React remaining polymers
      val newReactedLength = reactPolymers(filteredPolymers).length

      // Checking if it's the lowest sequence reached
      if (newReactedLength < minSequenceLength) {
        minSequenceLength = newReactedLength
      }
    }

    minSequenceLength
  }
}
