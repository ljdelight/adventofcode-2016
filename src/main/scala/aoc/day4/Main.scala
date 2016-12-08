package aoc.day4
import scala.io.Source
import collection.immutable.ListMap

object Main {

  def computeChecksum(name: String): String = {
    type Pair = (Char, Int)
    def compare(a: Pair, b: Pair): Boolean= {
      if (a._2 == b._2) a._1 < b._1
      else a._2 > b._2
    }

    ListMap(name.filterNot(_ == '-').groupBy(identity)
      .mapValues(_.length)
      .toSeq
      .sortWith(compare).take(5): _*)
      .keys.mkString
  }

  def main(args: Array[String]): Unit = {
    val filename = "/aoc/day4/in1.txt"
    val stream = getClass.getResourceAsStream(filename)
    val cleanedInput = Source.fromInputStream(stream).getLines().toList

    val roomRegex = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r
    val count = cleanedInput.foldLeft(0)((acc: Int, line: String) =>
      line match {
        case roomRegex(name, sectorId, checksum) if checksum.equals(computeChecksum(name)) => acc + sectorId.toInt
        case _ => acc
      }
    )

    println(s"Part 1: $count")



    val realRooms = cleanedInput.foldLeft(List[String]())((acc: List[String], line: String) =>
      line match {
        case roomRegex(name, sectorId, checksum) if checksum.equals(computeChecksum(name)) => acc :+ line
        case _ => acc
      }
    )

    def decrypt(name: String, sectorId: Int): String = {
      name.foldLeft("")((acc: String, thechar: Char) =>
        thechar match {
          case '-' => acc + " "
          case _ => acc + ('a'.toInt + (thechar.toInt - 'a'.toInt + sectorId.toInt) % 26).toChar
        }
      )
    }

    realRooms.foreach(_ match {
      case roomRegex(name, sectorId, checksum) => {
        val decrypted = decrypt(name, sectorId.toInt)
        if (decrypted.contains("storage")) println(decrypted + " " + sectorId)
      }
    })
  }
}
