package regex

import scala.None

class Group(val id: Int, group: Seq[Set[Char]]) {
  def append(char: Set[Char]): Group = new Group(id, group :+ char)
  def toNode: Node = {
    group.map{LiteralNode(_)}.foldRight(NilNode.asInstanceOf[Node]) {
      case (head: Node, tail:Node) => head then tail
    }
  }
}

object Group {
  def apply(id: Int): Group = new Group(id, Seq())
}

trait Node {
  def accept(s: Seq[Set[Char]]): Boolean = {
    accept(s, Seq(), Map())
  }
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroupMatchers: Map[Int, Node]): Boolean
  def then(next: Node): Node
  def append(tail: Node): Node
  def ::(prev: Node): Node = prev then this
  def or(other: Node): OrNode = new OrNode(Seq(this, other), NilNode)
}

object NilNode extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = true
  def then(next: Node): Node = throw new IllegalStateException("Nil node cannot precede anything")
  def append(tail: Node): Node = tail match {
    case NilNode => NilNode
    case TerminalNode => TerminalNode
    case _ => tail append NilNode
  }
}

object TerminalNode extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = s.isEmpty
  def then(next: Node) = throw new IllegalArgumentException("Cannot provide a next node to a TerminalNode")
  def append(tail: Node): Node = tail match {
    case NilNode => TerminalNode
    case TerminalNode => TerminalNode
    case _ => tail append TerminalNode
  }
}

class LiteralNode(chars: Set[Char], inverted: Boolean, next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = {
    s match {
      case c :: rest =>
        val matchingChars = if (inverted) c -- chars else chars & c
        !matchingChars.isEmpty && next.accept(rest, openGroups.map {_.append(matchingChars)}, closedGroups)
      case Nil => false
    }
  }
  def then(next: Node): Node = new LiteralNode(chars,inverted, next)
  def append(tail: Node): Node = this then next.append(tail)
}

object LiteralNode {
  def apply(chars: Set[Char]) = new LiteralNode(chars, false, NilNode)
  def apply(chars: Set[Char], inverted: Boolean) = new LiteralNode(chars, inverted, NilNode)
  def apply(char: Char) = new LiteralNode(Set(char), false, NilNode)
  def apply(char: Char, inverted: Boolean) = new LiteralNode(Set(char), inverted, NilNode)
}

class WildcardNode(next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = {
    if (s.isEmpty) false else next.accept(s.tail, openGroups.map{_.append(s.head)}, closedGroups)
  }

  def then(next: Node): Node = new WildcardNode(next)
  def append(tail: Node): Node = this then next.append(tail)
}

object WildcardNode {
  def apply() = new WildcardNode(NilNode)
}

class RepeatNode(self: Node, next: Node, min: Int, max: Option[Int]) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = {
    if (min > 0) {
      self append selfAccepted() accept(s, openGroups, closedGroups)
    } else  {
      max match {
        case Some(mx) if mx == 0 => next.accept(s, openGroups, closedGroups)
        case _ => (self append selfAccepted() accept(s, openGroups, closedGroups)) || next.accept(s, openGroups, closedGroups)
      }
    }
  }

  def then(next: Node): Node = new RepeatNode(self, next, min, max)
  def append(tail: Node): Node = this then next.append(tail)
  def selfAccepted(): Node = new RepeatNode(self, next, min-1, max.map{_-1})
}

object RepeatNode {
  def apply(repeated: Node) = new RepeatNode(repeated, NilNode, 0, None)
  def apply(repeated: Node, min: Int) = new RepeatNode(repeated, NilNode, min, None)
  def apply(repeated: Node, min: Int, max: Int) = new RepeatNode(repeated, NilNode, min, Some(max))
  def apply(repeated: Node, min: Int, max: Option[Int]) = new RepeatNode(repeated, NilNode, min, max)
}

class OrNode(nodes: Seq[Node], next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = {
    !nodes.toStream.map {
      _.append(next).accept(s, openGroups, closedGroups)
    }.dropWhile{!_}.isEmpty
  }

  def then(next: Node): Node = new OrNode(nodes, next)
  override def or(node: Node): OrNode = new OrNode(node +: nodes, next)
  def append(tail: Node): Node = this then next.append(tail)
}

object OrNode {
  def apply(): OrNode = new OrNode(Seq(), NilNode)
  def apply(node: Node): OrNode = new OrNode(Seq(node), NilNode)
}

class BackrefNode(group: Int, next: Node) extends Node{
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroups: Map[Int, Node]): Boolean = {
    closedGroups.getOrElse(group, PassNode()).append(next).accept(s, openGroups, closedGroups)
  }

  def then(next: Node): Node = new BackrefNode(group, next)
  def append(tail: Node): Node = this then next.append(tail)
}

object BackrefNode {
  def apply(group: Int) = new BackrefNode(group, NilNode)
}

class OpenGroupNode(group: Int, next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroupMatchers: Map[Int, Node]): Boolean = {
    next.accept(s, Group(group) +: openGroups, closedGroupMatchers)
  }

  def then(next: Node): Node = new OpenGroupNode(group, next)
  def append(tail: Node): Node = this then next.append(tail)
}

object OpenGroupNode {
  def apply(group: Int): OpenGroupNode = new OpenGroupNode(group, NilNode)
}

class CloseGroupNode(next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroupMatchers: Map[Int, Node]): Boolean = {
    val closingGroup = openGroups.head
    next.accept(s, openGroups.tail, closedGroupMatchers + (closingGroup.id -> closingGroup.toNode))
  }

  def then(next: Node): Node = new CloseGroupNode(next)
  def append(tail: Node): Node = this then next.append(tail)
}

object CloseGroupNode {
  def apply(): CloseGroupNode = new CloseGroupNode(NilNode)
}

class PassNode(next: Node) extends Node {
  def accept(s: Seq[Set[Char]], openGroups: Seq[Group], closedGroupMatchers: Map[Int, Node]): Boolean = {
    next.accept(s, openGroups, closedGroupMatchers)
  }

  def then(next: Node): Node = new PassNode(next)
  def append(tail: Node): Node = this then next.append(tail)
}

object PassNode {
  def apply(): PassNode = new PassNode(NilNode)
}
