package Day04

import scala.io.Source


class Day04 {
  println("day04")

  case class Nap(var start: Int, var end: Int, var guardId: Int) {
    override def toString: String = s"($guardId napped from $start to $end)"
    def getTimeNapped: Int = end - start
    def getMinutesAsleep: Array[Int] = (start until end).toArray
  }

  def readNaps(filename : String) : Array[Nap] = {
    // Read input file
    val input = Source.fromFile(filename)
    val lines : Array[String] = (for (line <- input.getLines()) yield line).toArray
    val sortedRecords = lines.sortWith(_ < _)

    // Regex to match the minute
    val recordPattern = """\[(.{16})\] (.*)""".r
    val minutePattern = """\d{4}-\d{2}-\d{2} \d{2}:(\d{2})""".r
    val beginShiftPattern = """Guard #(\d+).*""".r

    var naps : Array[Nap] = Array[Nap]()
    var guardId : String = "-1"
    var startNap : Int = 0

    // Loop through file lines
    for (line <- sortedRecords) {
      var recordPattern(timestamp, actionStr) = line
      var minutePattern(minute) = timestamp

      actionStr match {
        case "falls asleep" => {
          startNap = minute.toInt
        }
        case "wakes up" => {
          naps +:= Nap(startNap, minute.toInt, guardId.toInt)
        }
        case beginShiftPattern(guard) => {
          guardId = guard
        }
      }
    }

    naps
  }

  def star1(): Int = {
    // Read naps file
    val naps : Array[Nap] = readNaps("src/main/scala/Day04/input04.txt")

    // Group naps by guard
    val napsPerGuard : Map[Int, Array[Nap]] = naps.groupBy(_.guardId)

    // Find laziest guard
    var laziestGuardId : Int = -1
    var largestTimeNapped : Int = -1
    napsPerGuard.foreach(tuple => {
      val (guard, napList) = tuple
      val currentTimeNapped = napList.foldLeft(0)((sum, currentNap) => currentNap.getTimeNapped + sum)

      if (currentTimeNapped > largestTimeNapped) {
        largestTimeNapped = currentTimeNapped
        laziestGuardId = guard
      }
    })

    // Find the minute
    val minutesAsleep = napsPerGuard(laziestGuardId).flatMap(nap => nap.getMinutesAsleep).toArray
    val mostCommonMinute = minutesAsleep.groupBy(identity).maxBy(_._2.length)._1

    mostCommonMinute * laziestGuardId
  }

  def star2() : Int = {
    // Read naps file
    val naps : Array[Nap] = readNaps("src/main/scala/Day04/input04.txt")

    // Group naps by guard
    val napsPerGuard : Map[Int, Array[Nap]] = naps.groupBy(_.guardId)

    // Results
    var laziestGuardId : Int = -1
    var minuteFoundAsleepTheMost : Int = -1
    var timesFoundAsleepInMinute: Int = -1

    // Cycle through naps of every guard
    napsPerGuard.foreach(tuple => {
      val (guard, napList) = tuple
      val minutesAsleep = napList.flatMap(nap => nap.getMinutesAsleep).toArray.groupBy(identity)
      val mostCommonMinuteGroup = minutesAsleep.maxBy(_._2.length)

      val (minute, occurrences) = mostCommonMinuteGroup

      if (occurrences.length > timesFoundAsleepInMinute) {
        minuteFoundAsleepTheMost = minute
        timesFoundAsleepInMinute = occurrences.length
        laziestGuardId = guard
      }
    })

    laziestGuardId * minuteFoundAsleepTheMost
  }
}
