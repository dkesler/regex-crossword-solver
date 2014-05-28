import regex.{Node, Parser}
import scala.util.matching.{Regex => SRegex}

class Regex(r: String) {
  val node: Node = Parser.compile(r)
  def accepts(s: String): Boolean = {
    node.accept(s.map{Set(_)}.toList)
    /*new SRegex(r).findFirstIn(s) match {
      case Some(m) => true
      case None => false
    }*/
  }

  def accepts(s: List[Set[Char]]): Boolean = {
    node.accept(s)
  }
}
