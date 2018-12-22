package Day07
import scala.io.Source


class Day07 {

  def readInput(filename : String) : Array[(Char, String)] = {
    // Read input file
    val input = Source.fromFile(filename)

    // Regex for parsing each line
    val pattern = """^Step (\w) must be finished before step (\w) can begin.$""".r

    // Map of step and dependencies
    var dependencies : Array[(Char, String)] = Array[(Char, String)]()

    // Read input
    for (line <- input.getLines()) {
      val pattern(step, newDependency) = line

      // Check if current line's task is already defined
      dependencies = dependencies.find(_._1 == step(0)) match {
        case Some(tuple) =>
          // if task is already defined, update the dependency string
          val (_, deps) = tuple
          dependencies.filterNot(_._1 == step(0)) :+ (step(0), s"$deps$newDependency")
        case None =>
          // if task is not defined, add it to dependencies array
          dependencies :+ (step(0), newDependency)
      }
    }

    dependencies
  }

  def star1() : String = {
    var dependencies = readInput("src/main/scala/Day07/input07.txt")
    var tasks = (dependencies.flatMap(_._2).distinct ++ dependencies.map(_._1)).distinct

    var finalString = ""
    val finalStringLength = tasks.length

    while (finalString.length < finalStringLength) {
      // Gather available tasks (with no dependency left
      val availableTasks = tasks.filterNot(dependencies.flatMap(_._2).distinct.contains(_)).sortWith(_ < _)

      // Update dependencies and tasks arrays
      tasks = tasks.filterNot(_ == availableTasks(0))
      dependencies = dependencies.filterNot(_._1 == availableTasks(0))

      // Update result string
      finalString = finalString + availableTasks(0)
    }

    finalString
  }

  case class Task (id: Char, dependencies : String, workerId: Int, duration: Int, remainingDuration: Int) {
    def tickTime(): Task = copy(remainingDuration = remainingDuration - 1)
  }

  def star2() : Int = {
    var input = readInput("src/main/scala/Day07/input07.txt")
    val noWorkers = 5
    val baseTime = 61

    var finalString = ""

    // Data structures that will mutate on iteration
    var tasks = input.map(value => Task(value._1, value._2, -1, (value._1 - 'A' + baseTime), (value._1 - 'A' + baseTime)))
    var workers = Array.fill[Boolean](noWorkers)(false)

    // Time counter
    var t : Int = 0
    while (tasks.length > 0) {

      // Get tasks that are ready to start execution, excluding tasks already running
      val availableTasks = tasks.filterNot(t => tasks.flatMap(_.dependencies).distinct.contains(t.id) && t.workerId < 0).sortWith(_.id < _.id)
      val assignableTasks = availableTasks.filter(_.workerId == -1)

      // Loop through all tasks
      for (taskIndex <- tasks.indices) {

        // Only try to assign assignableTasks, ignoring the rest of tasks
        if (assignableTasks.contains(tasks(taskIndex))) {

          // Gather available workers, if any, assign to current assignableTask and flag worker as occupied
          val currentAvailableWorkers = workers.zipWithIndex.filter(_._1 == false).filterNot(tasks.map(t => t.workerId).contains(_))
          if (currentAvailableWorkers.length > 0) {
            val workerIndex = currentAvailableWorkers.head._2
            workers(workerIndex) = true
            tasks(taskIndex) = tasks(taskIndex).copy(workerId = workerIndex)
          }
        }
      }

      // Tick time for workers
      for (taskIndex <- tasks.indices) {
        if (tasks(taskIndex).workerId > -1) {
          tasks(taskIndex) = tasks(taskIndex).tickTime()
          // println(s"${tasks(taskIndex).workerId} is working on ${tasks(taskIndex).id} (missing ${tasks(taskIndex).remainingDuration})")
        }
      }
      t = t + 1

      // Check if anything ended. If it did, remove from tasks array, free the worker and update final string
      val endedTasks = tasks.filter(_.remainingDuration == 0)
      if (endedTasks.length > 0) {
        // println(s"\t\tfinished ${endedTasks.mkString("")}")
        for (t <- endedTasks) {
          workers(t.workerId) = false
          finalString = finalString + endedTasks.map(_.id).mkString("")
        }
        tasks = tasks.filter(_.remainingDuration > 0)
      }
    }

    // Add time that takes to finish final task
    val distinctTasks = (input.flatMap(_._2).distinct ++ input.map(_._1)).distinct
    t = t + (distinctTasks.filterNot(finalString.contains(_)).sortWith(_ < _)(0) - 'A' + baseTime)
    t
  }
}
