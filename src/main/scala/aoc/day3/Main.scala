package aoc.day3
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val filename = "/aoc/day3/in1.txt"
    val stream = getClass.getResourceAsStream(filename)
    val cleanedInput: List[Array[Int]] = Source.fromInputStream(stream).getLines().map(_.trim().split("\\s+").map(_.toInt)).toList

    val part1 = cleanedInput.foldLeft(0)((acc: Int, values: Array[Int]) => {
      val s = values.sorted
      if (s(0) + s(1) > s(2)) acc + 1
      else acc
    })
    println(part1)


    // the row number, triangle count, and 3 triangles
    type State = (Int, Int, List[Int], List[Int], List[Int])

    def isTriangle(values: List[Int]) = {
      //  println(values)
      val s = values.sorted
      if (s(0) + s(1) > s(2)) 1
      else 0
    }

    val part2 = cleanedInput.foldLeft((1, 0, List[Int](), List[Int](), List[Int]()))(
      (acc: State, values: Array[Int]) => {
        if (acc._1 % 3 == 0) {
          (acc._1 + 1,
            acc._2 + isTriangle(values(0) :: acc._3) + isTriangle(values(1) :: acc._4) + isTriangle(values(2) :: acc._5),
            List(), List(), List())
        } else {
          (acc._1 + 1,
            acc._2,
            values(0) :: acc._3, values(1) :: acc._4, values(2) :: acc._5)

        }
      }
    )

    println(part2._2)
  }
}
