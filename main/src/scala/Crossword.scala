
class Crossword(rows: IndexedSeq[Regex], cols: IndexedSeq[Regex], val cells: IndexedSeq[IndexedSeq[Cell]], val correctSolution: Option[IndexedSeq[IndexedSeq[Char]]]) {
type Cells = IndexedSeq[IndexedSeq[Cell]]
  def solve(): Option[Cells] = {
    val updatedCells = removeInvalidOptions(cells)
    if (isSolved(updatedCells))
      Some(updatedCells)
    else if (isImpossible(updatedCells))
      None
    else {
      branch(updatedCells)
    }
  }

  private def branch(cells: Cells): Option[Cells] = {
    def findFirstUndetermined: (Int, Int) = {
      def findFirstUndeterminedIter(r: Int, c: Int): (Int, Int) = {
        if (r == rows.size)
          throw new Error("Tried to branch on solution without choices")
        else if ( c == cols.size)
          findFirstUndeterminedIter(r+1, 0)
        else if (cells(r)(c).domain.size > 1)
          (r, c)
        else
          findFirstUndeterminedIter(r, c+1)
      }

      findFirstUndeterminedIter(0, 0)
    }
    val (r, c) = findFirstUndetermined

    def branchIter(domain: IndexedSeq[Char]): Option[Cells] = {
      if (domain.isEmpty)
        None
      else {
        new Crossword(rows, cols, updateCells(new Cell(Set(domain.head)), r, c, cells), correctSolution).solve() match {
          case Some(solvedCells) => Some(solvedCells)
          case None => branchIter(domain.tail)
        }
      }
    }

    branchIter(cells(r)(c).domain.toIndexedSeq)
  }

  private def updateCells(newCell: Cell, r: Int, c: Int, cells: Cells): Cells = {
    cells.updated(r, cells(r).updated(c, newCell))
  }

  private def removeInvalidOptions(cells: Cells): Cells = {
    val updatedCells = invalidOptionRemovalPass(cells)
    if (updatedCells.flatten.zip(cells.flatten).count(i => i._1.domain != i._2.domain) == 0)
      updatedCells
    else
      removeInvalidOptions(updatedCells)
  }
  
  private def invalidOptionRemovalPass(cells: Cells): Cells = {
    def cullDomain(cells: Cells, r: Int, c: Int): Cell = {
      val rowPrefix = cells(r).take(c).map{_.domain}.toList
      val rowSuffix = cells(r).drop(c+1).map { _.domain }.toList
      val rowRegex = rows(r)
      val col = cells.map { _(c) }
      val colPrefix = col.take(r).map{ _.domain }.toList
      val colSuffix = col.drop(r+1).map{ _.domain }.toList
      val colRegex = cols(c)

      def cullDomainIter(domain: IndexedSeq[Char], validDomain: Set[Char]): Cell = {
        if (domain.isEmpty)
          new Cell(validDomain)
        else if (rowRegex.accepts(rowPrefix ++ (Set(domain.head) +: rowSuffix)) && colRegex.accepts(colPrefix ++ (Set(domain.head) +: colSuffix)))
          cullDomainIter(domain.tail, validDomain + domain.head)
        else
          cullDomainIter(domain.tail, validDomain)
      }

      cullDomainIter(cells(r)(c).domain.toIndexedSeq, Set())
    }

    def removeInvalidOptionsForCell(cells: Cells, r: Int, c: Int): Cells = {
      if (r == rows.size)
        cells
      else if ( c == cols.size)
        removeInvalidOptionsForCell(cells, r+1, 0)
      else {
        val updatedCell = cullDomain(cells, r, c)
        removeInvalidOptionsForCell(updateCells(updatedCell, r, c, cells), r, c+1)
      }
    }

    removeInvalidOptionsForCell(cells, 0, 0)
  }

  private def isSolved(cells: Cells): Boolean = {
    cells.flatten.forall(_.domain.size == 1)
  }

  private def isImpossible(cells: Cells): Boolean = {
    !cells.flatten.filter{ _.domain.isEmpty }.isEmpty
  }
}

object Crossword {

  def apply(rows: IndexedSeq[Regex], cols: IndexedSeq[Regex]): Crossword = {
    new Crossword(
      rows,
      cols,
      IndexedSeq.fill(rows.size)(IndexedSeq.fill(cols.size)(Cell())),
      None
    )
  }

  def apply(rows: IndexedSeq[Regex], cols: IndexedSeq[Regex], correctSolution: IndexedSeq[IndexedSeq[Char]]): Crossword = {
    new Crossword(
      rows,
      cols,
      IndexedSeq.fill(rows.size)(IndexedSeq.fill(cols.size)(Cell())),
      Some(correctSolution)
    )
  }
}


