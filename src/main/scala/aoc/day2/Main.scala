package aoc.day2

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    val filename = "/aoc/day2/in1.txt"
    val stream = getClass.getResourceAsStream(filename)
    val cleanedInput: Iterator[String] = Source.fromInputStream(stream).getLines()

    type Accumulator = (String, (Int, Int))
    val intial: Accumulator = ("", (1, 1))

    val res = cleanedInput.foldLeft(intial: Accumulator)((acc, instructionLine)  => {
      def determineKeyForLine(line: String, pos: (Int, Int)): (Int, Int) = {
        val res = line.foldLeft(pos)((p: (Int, Int), thechar: Char) => {
          // println(thechar)
          thechar match {
            case 'U' => (math.max(0, p._1 - 1), p._2)
            case 'D' => (math.min(2, p._1 + 1), p._2)
            case 'L' => (p._1, math.max(0, p._2 - 1))
            case 'R' => (p._1, math.min(2, p._2 + 1))
          }
        })
        // println(res)
        res
      }

      val thekey = determineKeyForLine(instructionLine, acc._2)
      (acc._1 + (thekey._1 * 3 + thekey._2 + 1).toString, thekey)
    })

    println(res._1)

  }
}
