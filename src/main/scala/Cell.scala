class Cell(val domain: Set[Char]) {
  def getRepresentative: Char = {
    if (domain.size != 1) '.' else domain.head
  }

  override def toString: String = {
    if (domain.size == 1) domain.head.toString else domain.toString()
  }

}

object Cell {
  def apply(): Cell = {
    new Cell(regex.validChars)
  }
}
