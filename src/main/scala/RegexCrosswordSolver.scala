import java.io.File

object RegexCrosswordSolver extends App {
  override def main(args: Array[String]): Unit = {
    val url = getClass.getResource(args(0))
    if (url == null) throw new Error("Could not locate file " + args(0))
    val file = new File(url.getFile)
    val crossword = PuzzleReader.read(file)
    val sol = crossword.solve()

    sol match {
      case Some(cells) =>
        val formattedSolution = cells.map{_.mkString(" ")}.mkString("\n")
        println(formattedSolution)
        crossword.correctSolution match {
          case Some(correctSol) =>
            val formattedCorrectSolution = correctSol.map{_.mkString(" ")}.mkString("\n")
            if (formattedCorrectSolution.equals(formattedSolution))
              println("Solution is correct")
            else
              println(s"Solution is incorrect.  Correct Solution:\n$formattedCorrectSolution")
          case None => println("Unknown if the solution is correct")
        }
      case None =>
        crossword.correctSolution match {
          case Some(correctSol) =>
            val formattedCorrectSolution = correctSol.map{_.mkString(" ")}.mkString("\n")
            println(s"Failed to solve.  Correct Solution:\n$formattedCorrectSolution")
          case None =>
            println("Failed to solve.  Correct solution is unknown")
        }
    }
  }
}
