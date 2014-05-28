import org.junit.runner.RunWith
import org.scalatest.FunSuite
import java.io.File
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FullPuzzleTests extends FunSuite {

  test("beginner-1.dat") {
    testPuzzle("puzzles/regexcrossword/beginner-1.dat")
  }
  test("beginner-2.dat") {
    testPuzzle("puzzles/regexcrossword/beginner-2.dat")
  }
  test("beginner-3.dat") {
    testPuzzle("puzzles/regexcrossword/beginner-3.dat")
  }
  test("beginner-4.dat") {
    testPuzzle("puzzles/regexcrossword/beginner-4.dat")
  }
  test("beginner-5.dat") {
    testPuzzle("puzzles/regexcrossword/beginner-5.dat")
  }
  test("intermediate-1.dat") {
    testPuzzle("puzzles/regexcrossword/intermediate-1.dat")
  }
  test("intermediate-2.dat") {
    testPuzzle("puzzles/regexcrossword/intermediate-2.dat")
  }
  test("intermediate-3.dat") {
    testPuzzle("puzzles/regexcrossword/intermediate-3.dat")
  }
  test("intermediate-4.dat") {
    testPuzzle("puzzles/regexcrossword/intermediate-4.dat")
  }
  test("intermediate-5.dat") {
    testPuzzle("puzzles/regexcrossword/intermediate-5.dat")
  }

  def testPuzzle(puzzle: String) {
    val url = getClass.getResource(puzzle)
    val file = new File(url.getFile)
    val crossword = PuzzleReader.read(file)
    val sol = crossword.solve()
    assert(sol.isDefined, "Correct solution should have been reached")

    val correctSol = crossword.correctSolution.get

    assert(sol.get.map {
      _.map {
        _.getRepresentative
      }
    }.equals(correctSol))
  }
}
