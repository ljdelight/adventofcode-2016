package aoc.day1

import scala.io.Source

object Main {

  abstract case class Direction(x: Int, y: Int) {
    def left(mag: Int): Direction
    def right(mag: Int): Direction
    def forward(mag: Int): Direction
    override def toString: String = s"($x , $y)"
  }

  class North(x: Int, y: Int) extends Direction(x, y) {
    def left(mag: Int) = new West(x - mag, y)
    def right(mag: Int) = new East(x + mag, y)
    def forward(mag: Int) = new North(x, y + mag)
  }

  class East(x: Int, y: Int) extends Direction(x, y) {
    def right(mag: Int) = new South(x, y - mag)
    def left(mag: Int) = new North(x, y + mag)
    def forward(mag: Int) = new East(x + mag, y)
  }

  class South(x: Int, y: Int) extends Direction(x, y) {
    def right(mag: Int) = new West(x - mag, y)
    def left(mag: Int) = new East(x + mag, y)
    def forward(mag: Int) = new South(x, y - mag)
  }

  class West(x: Int, y: Int) extends Direction(x, y) {
    def right(mag: Int) = new North(x, y + mag)
    def left(mag: Int) = new South(x, y - mag)
    def forward(mag: Int) = new West(x - mag, y)
  }


  def main(args: Array[String]): Unit = {

    val filename = "/aoc/day1/in1.txt"
    val stream = getClass.getResourceAsStream(filename)
    val cleanedInput: Array[String] = Source.fromInputStream(stream).mkString.split(",").map(_.trim)

    // Solve day 1 part 1
    val finalLocation = cleanedInput.foldLeft(new North(0,0): Direction)((whereami: Direction, instruction)  => {
      val dirChar = instruction.charAt(0)
      val magnitude = instruction.substring(1).toInt
      dirChar match {
        case 'R' => whereami.right(magnitude)
        case 'L' => whereami.left(magnitude)
      }
    }
    )

    println(s"Blocks from HQ: ${finalLocation.x.abs + finalLocation.y.abs}")


    // Solve day 1 part 2
    //
    type Visited = List[(Int, Int)]

    val hq = cleanedInput.foldLeft((new North(0,0), List((0,0)) ): (Direction, Visited)) {
      (whereami: (Direction, Visited), instruction) => {
        def moveOnEdge(d: Direction, acc: Visited, m: Int): (Direction, Visited) = {
          if (m > 0) {
            val p = d.forward(1)
            moveOnEdge(p, acc :+ (p.x, p.y), m - 1)
          }
          else (d, acc)
        }

        val dirChar = instruction.charAt(0)
        val magnitude = instruction.substring(1).toInt

        dirChar match {
          case 'R' => {
            val p = whereami._1.right(0)
            moveOnEdge(p, whereami._2, magnitude)
          }
          case 'L' => {
            val p = whereami._1.left(0)
            moveOnEdge(p, whereami._2, magnitude)
          }
        }
      }
    }

    val visited = hq._2
    val solutions = visited.groupBy(identity).filter { case (_, xs) => xs.size > 1}.keys.toList

    println("First time crossing the same point: ")
    println(visited.diff(visited.diff(solutions))(0))
  }
}
