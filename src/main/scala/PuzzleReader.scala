import java.io.File
import scala.io.Source

object PuzzleReader {
  def read(file: File): Crossword = {
    val lines = Source.fromFile(file).getLines().toList

    val (height, width) = lines.head.split(" ").map(Integer.parseInt).toList match {
      case h :: w :: Nil => (h, w)
      case _ => throw new Error(s"Exception occurred parsing header $lines.head")
    }

    val startOfRowRegexes = lines.tail.dropWhile(_.isEmpty)
    val rowRegexes = startOfRowRegexes.take(height).toIndexedSeq.map(mkPuzzleRegex)

    val startOfColRegexes = startOfRowRegexes.drop(height).dropWhile(_.isEmpty)
    val colRegexes = startOfColRegexes.take(width).toIndexedSeq.map(mkPuzzleRegex)

    val startOfSolution = startOfColRegexes.drop(width).dropWhile(_.isEmpty)
    val solution = parseSolution(startOfSolution)

    Crossword(
      rowRegexes,
      colRegexes,
      solution
    )
  }

  private def parseSolution(solution: List[String]): IndexedSeq[IndexedSeq[Char]] = {
    solution.takeWhile(!_.isEmpty).map{_.toList.toIndexedSeq}.toIndexedSeq
  }

  private def mkPuzzleRegex(r: String): Regex = {
    val rWithStartMatch = if (r.startsWith("^")) r else "^" + r
    val rWithEndMatch = if (rWithStartMatch.endsWith("$")) rWithStartMatch else rWithStartMatch + "$"
    new Regex(rWithEndMatch)
  }
}
