package aoc.day5
import scala.io.Source
import collection.immutable.ListMap
import scala.annotation.tailrec

object Main {

  object MD5Chess {
    private val charmap = "0123456789abcdef"
    private val instance = java.security.MessageDigest.getInstance("MD5")
    @tailrec def md5(s: String, iteration: Int): (Int, Array[Byte]) = {
      val bytes = instance.digest((s + iteration.toString).getBytes)
      if (bytes(0) == 0 && bytes(1) == 0 && (bytes(2) & 0xf0) == 0) {
        (iteration, bytes)
      } else {
        md5(s, iteration + 1)
      }
    }
    @tailrec def solvePart1(s: String, iteration: Int, acc: String): String = {
      if (acc.length == 8) acc
      else {
        val res: (Int, Array[Byte]) = md5(s, iteration)
        solvePart1(s, res._1 + 1, acc + charmap(res._2(2)))
      }
    }
    // accumulator is a list of (idx, char) pairs to reconstruct the orig msg
    @tailrec def solvePart2(s: String, iteration: Int, acc: List[(Int, Char)]): String = {
      if (acc.length == 8) acc.sortBy(identity).map(_._2).mkString
      else {
        val res: (Int, Array[Byte]) = md5(s, iteration)
        val idx = res._2(2).toInt
        if (idx < 8 && !acc.exists(p => p._1 == idx)) solvePart2(s, res._1 + 1, (idx, charmap(0x0f & (res._2(3) >> 4))) :: acc)
        else solvePart2(s, res._1 + 1, acc)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val start = "ffykfhsq"
    println(MD5Chess.solvePart1(start, 0, ""))
    println(MD5Chess.solvePart2(start, 0, List()))

  }
}
