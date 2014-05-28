package regex

object Parser {
  val escapedChars = Set('.', '\\', '*', '+', '[', ']', '{', '}', '$', '^')
  val digits = ('0' to '9').toSet
  val whitespace = Set(' ', '\t', '\r', '\n', '\f')
  val wordChars = ('a' to 'z').toSet ++ ('A' to 'Z').toSet ++ ('0' to '9').toSet + '_'
  val classes = List(
    'd' -> digits,
    's' -> whitespace,
    'w' -> wordChars,
    'D' -> (regex.validChars -- digits),
    'S' -> (regex.validChars -- whitespace),
    'W' -> (regex.validChars -- wordChars)
  ).toMap

  def parse(regex: String): Token = {
    parseStartOfLine(regex.toList)
  }

  def compile(regex: String): Node = {
    toNode(parseStartOfLine(regex.toList))
  }

  private def parseStartOfLine(regex: List[Char]): RootToken = {
    regex.toList match {
      case '^' :: tail => parseEndOfLine(StartOfLineToken(true), regex.tail)
      case r => parseEndOfLine(StartOfLineToken(false), regex)
    }
  }

  private def parseEndOfLine(sol: StartOfLineToken, regex: List[Char]): RootToken = {
    regex.reverse match {
      case '$' :: head => RootToken(sol, EndOfLineToken(true), parseBody(head.reverse, 0)._1)
      case r => RootToken(sol, EndOfLineToken(false), parseBody(regex, 0)._1)
    }
  }

  private def parseBody(regex: List[Char], groupCount: Int): (ConsumingToken, Int) = {
    val (orTokens, newGroupCount) = parseOrTokens(regex, groupCount)
    orTokens match {
      case single :: Nil => (single, newGroupCount)
      case many => (OrToken(many), newGroupCount)
    }
  }

  private def parseOrTokens(regex: List[Char], initialGroupCount: Int): (Seq[ConsumingToken], Int) = {
    val orStrings = splitOrTokens(regex)
    orStrings.foldLeft((Seq[ConsumingToken](), initialGroupCount)) {
      case ((tokens, groupCount), regex: List[Char]) =>
      val (newTokens, newGroupCount) = parseGroupTokens(regex, groupCount)
        (tokens :+ newTokens, newGroupCount)
    }
  }

  private def splitOrTokens(regex: List[Char]): Seq[List[Char]] = {
    def findSplitPoints(regex: List[Char], idx: Int, openGroupCount: Int, splitLocations: Seq[Int]): Seq[Int] = {
      if (regex.isEmpty)
        splitLocations :+ idx
      else
        regex.head match {
          case '(' => findSplitPoints(regex.tail, idx+1, openGroupCount+1, splitLocations)
          case ')' => findSplitPoints(regex.tail, idx+1, openGroupCount-1, splitLocations)
          case '|' if openGroupCount == 0 => findSplitPoints(regex.tail, idx+1, openGroupCount, splitLocations :+ idx)
          case '\\' => findSplitPoints(regex.tail.tail, idx+2, openGroupCount, splitLocations)
          case _ => findSplitPoints(regex.tail, idx+1, openGroupCount, splitLocations)
        }
    }
    val splitPoints = findSplitPoints(regex, 0, 0, Seq(-1))

    splitPoints.sliding(2).map {
      case start :: end :: Nil => regex.slice(start+1, end)
    }.toSeq
  }

  private def parseGroupTokens(regex: List[Char], initialGroupCount: Int): (ConsumingToken, Int) = {
    val groupStrings = splitGroupTokens(regex)
    val (newTokens, newGroupCount) = groupStrings.foldLeft((Seq[ConsumingToken](), initialGroupCount)) {
      case ((tokens, groupCount), regex: List[Char]) =>
        val (token, newGroupCount) = regex.head match {
          case '(' => parseGroup(regex, groupCount)
          case _ => (parseLiteralTokens(regex), groupCount)
        }
        (tokens :+ token, newGroupCount)
    }

    (ConcatToken(newTokens), newGroupCount)
  }

  // foo(ba(r))+[baz]* => [foo, (ba(r))+, [baz]*]
  private def splitGroupTokens(regex: List[Char]): Seq[List[Char]] = {
    def findSplitPoints(regex: List[Char], idx: Int, openGroupCount: Int, splitLocations: Seq[Int]): Seq[Int] = {
      if (regex.isEmpty)
        splitLocations :+ idx
      else
        regex.toList  match {
          case '\\' :: rest => findSplitPoints(regex.tail.tail, idx+2, openGroupCount, splitLocations)
          case '(' :: rest if openGroupCount == 0 => findSplitPoints(regex.tail, idx+1, openGroupCount+1, splitLocations :+ idx)
          case '(' :: rest if openGroupCount > 0 => findSplitPoints(regex.tail, idx+1, openGroupCount+1, splitLocations)
          case ')' :: rest if openGroupCount > 1  => findSplitPoints(regex.tail, idx+1, openGroupCount-1, splitLocations)
          case ')' :: rest if openGroupCount == 1 =>
            val quantifierLength = findQuantifierLength(rest)
            findSplitPoints(regex.slice(1+quantifierLength, regex.length), idx+1+quantifierLength, openGroupCount-1, splitLocations :+ idx+1+quantifierLength)
          case _ => findSplitPoints(regex.tail, idx+1, openGroupCount, splitLocations)
        }
    }
    val splitPoints = findSplitPoints(regex, 0, 0, Seq(0))

    splitPoints.sliding(2).map {
      case start :: end :: Nil => regex.slice(start, end)
    }.filter(!_.isEmpty).toSeq
  }

  private def findQuantifierLength(regex: List[Char]): Int = {
    regex match {
        case '+' :: rest => 1
        case '*' :: rest => 1
        case '?' :: rest => 1
        case '{' :: rest => regex.indexOf('}') + 1
        case _ => 0
      }
  }

  private def parseLiteralTokens(regex: List[Char]): ConsumingToken = {
    val literalStrings = splitLiteralTokens(regex)

    val newTokens = literalStrings.foldLeft(Seq[ConsumingToken]()) {
      case (tokens, token) =>
        tokens :+ (token.toList match {
          case '\\' :: escaped :: rest if digits.contains(escaped) => parseBackref(token)
          case '\\' :: escaped :: rest if classes.contains(escaped) => applyQuantifier(parseShorthandClass(escaped), rest)
          case '\\' :: escaped :: rest => applyQuantifier(LiteralToken(Set(escaped)), rest)
          case '[' :: rest => parseClass(token)
          case '.' :: rest => applyQuantifier(WildcardToken(), rest)
          case char :: rest => applyQuantifier(LiteralToken(Set(char)), rest)
        })
    }

    ConcatToken(newTokens)
  }

  private def parseShorthandClass(cls: Char): ConsumingToken = {
    LiteralToken(classes(cls))
  }

  private def splitLiteralTokens(regex: List[Char]): Seq[List[Char]] = {
    def findSplitPoints(regex: List[Char], idx: Int, classOpen: Boolean, splitLocations: Seq[Int]): Seq[Int] = {
      if (regex.isEmpty)
        splitLocations
      else if (classOpen)
        regex  match {
          case '\\' :: escaped :: rest => findSplitPoints(regex.tail.tail, idx+2, classOpen, splitLocations)
          case ']' :: rest =>
            val quantifierLength = findQuantifierLength(rest)
            findSplitPoints(regex.slice(1+quantifierLength, regex.length), idx+1+quantifierLength, false, splitLocations :+ 1+quantifierLength+idx)
          case char :: rest => findSplitPoints(regex.tail, idx+1, classOpen, splitLocations)
        }
      else
        regex.toList  match {
          case '\\' :: escaped :: rest if digits.contains(escaped) =>
            val backrefLength = findBackrefLength(escaped :: rest)
            val quantifierLength = findQuantifierLength(regex.slice(backrefLength+1, regex.length))
            findSplitPoints(regex.slice(1+quantifierLength+backrefLength, regex.length), idx+1+quantifierLength+backrefLength, classOpen, splitLocations :+ 1+quantifierLength+backrefLength+idx)
          case '\\' :: escaped :: rest =>
            val quantifierLength = findQuantifierLength(rest)
            findSplitPoints(regex.slice(2+quantifierLength, regex.length), idx+2+quantifierLength, classOpen, splitLocations :+ 2+quantifierLength+idx)
          case '[' :: rest => findSplitPoints(regex.tail, idx+1, true, splitLocations)
          case char :: rest =>
            val quantifierLength = findQuantifierLength(rest)
            findSplitPoints(regex.slice(1+quantifierLength, regex.length), idx+1+quantifierLength, classOpen, splitLocations :+ 1+quantifierLength+idx)
        }
    }
    val splitPoints = findSplitPoints(regex, 0, false, Seq(0))

    splitPoints.sliding(2).map {
      case start :: end :: Nil => regex.slice(start, end)
    }.toSeq
  }

  private def findBackrefLength(regex: List[Char]): Int = {
    def findBackrefLength(regex: List[Char], length: Int): Int = {
      regex match {
        case Nil => length
        case num :: rest if digits.contains(num) => findBackrefLength(rest, length+1)
        case nonNum :: rest => length
      }
    }

    findBackrefLength(regex, 0)
  }

  private def parseBackref(regex: List[Char]): ConsumingToken = {
    def parseGroupId(regex: List[Char], backref: String): (String, List[Char]) = {
      regex.toList match {
        case Nil => (backref, Nil)
        case x :: rest if digits.contains(x) => parseGroupId(regex.tail, backref + x)
        case quantifierStr => (backref, quantifierStr)
      }
    }

    val (groupId, quantifier) = parseGroupId(regex.tail, "")

    applyQuantifier(BackrefToken(Integer.parseInt(groupId)), quantifier)
  }

  private def parseClass(regex: List[Char]): ConsumingToken = {
    val (inverted, classCharsAsString) = regex match {
      case '[' :: '^' :: rest => (true, rest.slice(0, rest.lastIndexOf(']')))
      case '[' :: rest =>  (false, rest.slice(0, rest.lastIndexOf(']')))
      case _ => throw new Error(s"Exception occurred parsing class $regex: malformed class structure")
    }
    val quantifier = regex.slice(regex.lastIndexOf(']')+1, regex.length)

    val classCharsSplit = splitLiteralTokens(classCharsAsString)

    val classChars = classCharsSplit.foldLeft(Set[Char]()) {
      case (chars, charAsStr: List[Char]) =>
        chars ++ (charAsStr.toList match {
          case '\\' :: escaped :: Nil if classes.contains(escaped) => classes(escaped)
          case '\\' :: escaped :: Nil => Set(escaped)
          case char :: Nil => Set(char)
          case _ => throw new Error(s"Exception occurred parsing class $regex: Unrecognized character in class $charAsStr")
        })
    }

    applyQuantifier(LiteralToken(classChars, inverted), quantifier)
  }

  private def parseGroup(regex: List[Char], groupCount: Int): (ConsumingToken, Int)= {
    val groupStart = regex.indexOf('(')
    val groupEnd = regex.lastIndexOf(')')
    val (body, newGroupCount) = parseBody(regex.slice(groupStart+1, groupEnd), groupCount+1)

    (applyQuantifier(GroupToken(groupCount+1, body), regex.slice(groupEnd+1, regex.length)), newGroupCount)
  }

  private def applyQuantifier(token: ConsumingToken, quantifier: List[Char]): ConsumingToken = {
    quantifier match {
      case Nil => token
      case '+' :: rest => OneOrMoreToken(token)
      case '*' :: rest => ZeroOrMoreToken(token)
      case '?' :: rest => ZeroOrOneToken(token)
      case '{' :: rest =>
        if (rest.contains(',')) {
          val (min, max) = rest.slice(0, rest.indexOf('}')).splitAt(rest.indexOf(','))
          (min, max.tail) match {
            case (mn, Nil) => QuantityToken(token, Integer.parseInt(mn.mkString), None)
            case (mn, mx) => QuantityToken(token, Integer.parseInt(mn.mkString), Some(Integer.parseInt(mx.mkString)))
          }
        } else {
          val eq = Integer.parseInt(rest.slice(0, rest.indexOf('}')).mkString)
          QuantityToken(token, eq, Some(eq))
        }
      case err => throw new Error(s"Exception occurred applying quantifier: $quantifier")
    }
  }

  private def toNode(token: RootToken): Node = {
    token.toNode
  }
}

trait Token {
  def toNode: Node
}
trait ConsumingToken extends Token

case class RootToken(sol: StartOfLineToken, eol:EndOfLineToken, body: ConsumingToken) extends Token {
  override def toString = {
    s"$sol$body$eol"
  }

  def toNode: Node = {
    sol.toNode append body.toNode append eol.toNode
  }
}
case class StartOfLineToken(matchStart: Boolean) extends Token {
  override def toString = if (matchStart) "^" else ""
  def toNode: Node = {
    if (matchStart) PassNode() else RepeatNode(WildcardNode())
  }
}
case class EndOfLineToken(matchEnd: Boolean) extends Token {
  override def toString = if (matchEnd) "$" else ""

  def toNode: Node = {
    if (matchEnd) TerminalNode else NilNode
  }
}
case class OrToken(pieces: Seq[ConsumingToken]) extends ConsumingToken {
  override def toString = pieces.mkString("|")
  def toNode: Node = {
    pieces.foldLeft(OrNode()) {
      case (or, token) => or or token.toNode
    }
  }
}
case class GroupToken(id: Int, group: ConsumingToken) extends ConsumingToken {
  override def toString = s"($group)"
  def toNode: Node = {
    OpenGroupNode(id) append group.toNode append CloseGroupNode()
  }
}
case class OneOrMoreToken(literal: ConsumingToken) extends ConsumingToken {
  override def toString = s"$literal+"
  def toNode: Node = {
    val node = literal.toNode
    node append RepeatNode(node)
  }
}
case class ZeroOrMoreToken(literal: ConsumingToken) extends ConsumingToken {
  override def toString = s"$literal*"
  def toNode: Node = {
    RepeatNode(literal.toNode)
  }
}
case class ZeroOrOneToken(literal: ConsumingToken) extends ConsumingToken {
  override def toString = s"$literal?"
  def toNode: Node = {
    RepeatNode(literal.toNode, 0, 1)
  }
}

case class QuantityToken(literal: ConsumingToken, min: Int, max: Option[Int]) extends ConsumingToken {
  override def toString = {
    max match {
      case Some(mx) if mx != min => s"$literal{$min,${max.get}}"
      case Some(mx) if mx == min => s"$literal{$min}"
      case None => s"$literal{$min,}"
    }
  }

  def toNode: Node = {
    RepeatNode(literal.toNode, min, max)
  }

}
case class BackrefToken(id: Int) extends ConsumingToken {
  override def toString = s"\\$id"
  def toNode: Node = {
    BackrefNode(id)
  }
}

case class LiteralToken(chars: Set[Char], inverted: Boolean = false) extends ConsumingToken {
  def escape(char: Char): String = {
    if (Parser.escapedChars.contains(char)) s"\\$char" else s"$char"
  }
  override def toString = {
    if (chars.size == 1) s"${escape(chars.head)}"
    else if (inverted) s"[^${chars.map(escape).mkString}]"
    else s"[${chars.map(escape).mkString}]"
  }
  def toNode: Node = {
    LiteralNode(chars, inverted)
  }
}
case class WildcardToken() extends ConsumingToken {
  override def toString = "."
  def toNode: Node = {
    WildcardNode()
  }
}
case class ConcatToken(pieces: Seq[ConsumingToken]) extends ConsumingToken {
  override def toString = pieces.mkString
  def toNode: Node = {
    pieces.tail.foldLeft(pieces.head.toNode) {
      case (node, token) => node append token.toNode
    }
  }
}
