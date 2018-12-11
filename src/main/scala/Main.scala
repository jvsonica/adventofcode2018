import Day01._
import Day02._
import Day03._

object Main extends App {

  val d01 = new Day01()
  println("First star result is " + d01.star1(0))
  println("Second star result is " + d01.star2(0))

  val d02 = new Day02()
  println("First star result is " + d02.star1())
  println("Second star result is " + d02.star2())

  val d03 = new Day03()
  println("First star result is " + d03.star1())
  println("Second star result is " + d03.star2())
}
