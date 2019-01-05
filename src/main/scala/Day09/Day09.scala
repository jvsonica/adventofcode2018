package Day09
import java.util
import scala.io.Source


class Day09 {
  println("day09")

  def playMarblesDeque(players: Int, marbles: Int) : Array[Long] = {
    // Initialize data struts
    val scores = Array.fill[Long](players)(0)
    val ring = new util.LinkedList[Long]()
    ring.offerFirst(0)

    for (i <- 1 to marbles) {
      if (i % 23 == 0) {
        // Get current player, who will have its score increased
        val currentPlayerIndex = i % players
        scores(currentPlayerIndex) = scores(currentPlayerIndex) + i

        java.util.Collections.rotate(ring, 7)
        scores(currentPlayerIndex) = scores(currentPlayerIndex) + ring.removeLast()
        java.util.Collections.rotate(ring, -1)
      }
      else {
        java.util.Collections.rotate(ring, -1)
        ring.offerLast(i)
      }
    }

    scores
  }

  def playMarbles(players: Int, marbles: Int) : Array[Long] = {
    // Initialize data struts
    var scores : Array[Long] = Array.fill[Long](players)(0)
    var ring : Array[Long] = Array[Long]()
    ring :+= 0.toLong
    var currentMarbleIndex = 0

    for (i <- 1 to marbles) {
      // If the ring is too small, keep filling it
      if (ring.length < 2) {
        ring :+= i.toLong
        currentMarbleIndex = i
      }
      else if (i % 23 == 0) {
        // Get current player, who will have its score increased
        val currentPlayerIndex = i % players
        scores(currentPlayerIndex) = scores(currentPlayerIndex) + i

        // Take 7 marbles counterclockwise
        val marbleToRemoveIndex = if (currentMarbleIndex - 7 > 0) currentMarbleIndex - 7 else currentMarbleIndex -7 + ring.length

        // Update score
        scores(currentPlayerIndex) = scores(currentPlayerIndex) + ring(marbleToRemoveIndex)

        // Remove marble
        ring = ring.take(marbleToRemoveIndex) ++ ring.takeRight(ring.length - 1 - marbleToRemoveIndex)

        // Update currentMarble
        currentMarbleIndex = marbleToRemoveIndex
      }
      else {
        // Get the new marble position
        val newMarblePosition = (if (currentMarbleIndex + 1 > ring.length - 1) currentMarbleIndex + 1 - ring.length else currentMarbleIndex + 1) + 1

        // Update ring
        ring = ring.take(newMarblePosition) ++ Array[Long](i) ++ ring.takeRight(ring.length - newMarblePosition)

        // Update marble index
        currentMarbleIndex = newMarblePosition
      }
    }
    scores
  }

  def star1() : Long = {
    val input = Source.fromFile("src/main/scala/Day09/input09.txt").mkString

    val parseInput = """(\d*) players; last marble is worth (\d*) points""".r
    val parseInput(players, marbles) = input

    // 10 players; 1618 marbles: high score is 8317
    // 13 players; 7999 marbles: high score is 146373
    // 17 players; 1104 marbles: high score is 2764
    // 21 players; 6111 marbles: high score is 54718
    // 30 players; 5807 marbles: high score is 37305

    val scores = playMarbles(players.toInt, marbles.toInt)

    scores.max
  }

  def star2() : Long = {
    val input = Source.fromFile("src/main/scala/Day09/input09.txt").mkString

    val parseInput = """(\d*) players; last marble is worth (\d*) points""".r
    val parseInput(players, marbles) = input

    val scores = playMarbles(players.toInt, marbles.toInt * 100)
    scores.max
  }
}
