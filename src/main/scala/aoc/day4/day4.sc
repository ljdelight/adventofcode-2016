import collection.immutable.ListMap


type Pair = (Char, Int)
def compare(a: Pair, b: Pair): Boolean= {
  if (a._2 == b._2) a._1 < b._1
  else a._2 > b._2
}

def computeChecksum(name: String): String = {
  ListMap(name.filterNot(_ == '-').groupBy(identity)
    .mapValues(_.length)
    .toSeq
    .sortWith(compare).take(5): _*)
    .keys.mkString
}

val roomRegex = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r

//"aaaaa-bbb-z-y-x-123[abxyz]" match {
//  //case roomRegex(name, sectorId, checksum) => println(name + " " + sectorId + " " + checksum + " " + computeChecksum(name))
//  case roomRegex(name, sectorId, checksum) if checksum.equals(computeChecksum(name)) => "foo"
//}

import scala.io.Source

val filename = "/aoc/day4/in1.txt"
val stream = getClass.getResourceAsStream(filename)
val cleanedInput = Source.fromInputStream(stream).getLines().toList

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
      println(decrypted + " " + sectorId)
    }
  }
)