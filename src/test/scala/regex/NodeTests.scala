package regex

import org.scalatest.FunSuite

class NodeTests extends FunSuite {

  test("nil node should accept a blank string") {
    assert(NilNode.accept(Seq()))
  }

  test("A nil node should accept a non-blank string") {
    assert(NilNode.accept(Seq(Set('a'))))
  }

  test("A terminal node should accept a blank string") {
    assert(TerminalNode.accept(Seq()))
  }

  test("A terminal node should not accept a non-blank string") {
    assert(!TerminalNode.accept(Seq(Set('a'))))
  }

  test("A literal node should not accept a blank string") {
    assert(!LiteralNode('a').accept(Seq()))
  }

  test("A literal node should accept a string that begins with an accepted character") {
    assert(LiteralNode('a').accept(Seq(Set('a'))))
  }

  test("A literal node should not accept a string that does not begin with an accepted character") {
    assert(!LiteralNode('a').accept(Seq(Set('b'))))
  }

  test("An inverted literal node should not accept a string that begins with a character represented by the node") {
    assert(!LiteralNode('a', true).accept(Seq(Set('a'))))
  }

  test("A literal node should accept a string that does not begin with an character represented by the node") {
    assert(LiteralNode('a', true).accept(Seq(Set('b'))))
  }

  test("A literal node should pass the remainder of the string to the next node") {
    assert(!(LiteralNode('a') :: TerminalNode accept Seq(Set('a'), Set('b'))))
  }

  test("A wildcard node should not accept a blank string") {
    assert(!WildcardNode().accept(Seq()))
  }

  test("A wildcard node should accept a string starting with any character") {
    assert(WildcardNode().accept(Seq(Set('a'))))
  }

  test("A wildcard node should pass the remainder of the string to the next node") {
    assert(!(WildcardNode() :: TerminalNode accept Seq(Set('a'), Set('a'))))
  }

  test("A repeat node should accept a blank string") {
    assert(RepeatNode(LiteralNode('a')).accept(Seq()))
  }

  test("A repeat node should consume as many characters as possible if the remainder is accepted") {
    assert(RepeatNode(LiteralNode('a')) :: TerminalNode accept Seq(Set('a'), Set('a'), Set('a')))
  }

  test("A repeat node should not consume characters the repeated node does not accept") {
    assert(! (RepeatNode(LiteralNode('a')) :: TerminalNode accept Seq(Set('a'), Set('a'), Set('b'))))
  }

  test("A repeat node should not consume all characters if doing so causes the remainder to not be accepted") {
    assert( RepeatNode(LiteralNode('a')) :: LiteralNode('a') :: TerminalNode accept Seq(Set('a'), Set('a')))

  }

  test("A repeat node should not consume any characters if doing so causes the remainder to not be accepted") {
    assert( RepeatNode(LiteralNode('a')) :: LiteralNode('a') :: TerminalNode accept Seq(Set('a')))
  }

  test("An or node with no options should reject a blank string") {
    assert( !OrNode().accept(Seq()) )
  }

  test("An or node with no options should reject a non-blank string") {
    assert( !OrNode().accept(Seq(Set('a'))) )
  }

  test ("An or node with one option that accepts the string accepts the string") {
    assert( OrNode() or LiteralNode('a') accept Seq(Set('a')))
  }

  test ("An or node with one multi-token option that accepts the string accepts the string") {
    assert( OrNode() or (LiteralNode('a') :: LiteralNode('b')) accept Seq(Set('a'), Set('b')))
  }

  test ("An or node with several options, one of which accepts the string, accepts the string") {
    assert(LiteralNode('b') or LiteralNode('a') or LiteralNode('c') accept Seq(Set('a')))
  }

  test ("An or node which accepts the string passes the remainder on") {
    assert(! ((LiteralNode('b') or LiteralNode('a') or LiteralNode('c')) :: TerminalNode accept Seq(Set('a'), Set('b'))))
  }

  test ("An or node with several options, none of which accept the string, rejects the string") {
    assert(! (LiteralNode('b') or LiteralNode('c') or LiteralNode('d') accept Seq(Set('a'))))
  }

  //group backreference nodes
  test("(.)\1 aa") {
    assert( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('a')))
  }

  test("(.)\1 a[ab]") {
    assert( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('a', 'b')))
  }

  test("(.)\1 [ab][ab]") {
    assert( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: BackrefNode(1) :: TerminalNode accept Seq(Set('a', 'b'), Set('a', 'b')))
  }

  test("(.)+\1 aaa") {
    assert( RepeatNode(OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode()) :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('a'), Set('a')))
  }

  test("!  (.)+\1 aba") {
    assert(!( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: RepeatNode(OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode()) :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('b'), Set('a'))))
  }

  test("(.)+\1 baa") {
    assert( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: RepeatNode(OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode()) :: BackrefNode(1) :: TerminalNode accept Seq(Set('b'), Set('a'), Set('a')))
  }

  test("(.+)\1 aaaa") {
    assert(OpenGroupNode(1) :: RepeatNode(WildcardNode()) :: CloseGroupNode() :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('a'), Set('a'), Set('a')))
  }

  test("(.+)\1 ! aaa") {
    assert(! (OpenGroupNode(1) :: RepeatNode(WildcardNode()) :: CloseGroupNode() :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('a'), Set('a'))))
  }
  test("a(.)*\1a aa") {
    assert( LiteralNode('a') :: RepeatNode(OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode()) :: BackrefNode(1) :: LiteralNode('a') :: TerminalNode accept Seq(Set('a'), Set('a')))
  }

  test("a(.)*\1a abbba") {
    assert( LiteralNode('a') :: RepeatNode(OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode()) :: BackrefNode(1) :: LiteralNode('a') :: TerminalNode accept Seq(Set('a'), Set('b'), Set('b'), Set('b'), Set('a')))
  }

  test("(.)(.)\2\1 abba") {
    assert( OpenGroupNode(1) :: WildcardNode() :: CloseGroupNode() :: OpenGroupNode(2) :: WildcardNode() :: CloseGroupNode() :: BackrefNode(2) :: BackrefNode(1) :: TerminalNode accept Seq(Set('a'), Set('b'), Set('b'), Set('a')))
  }

  test("(.(.))\2\1 abbab") {
    assert( OpenGroupNode(1) :: WildcardNode() :: OpenGroupNode(2) :: WildcardNode() :: CloseGroupNode() :: CloseGroupNode() :: BackrefNode(2) :: BackrefNode(1) :: TerminalNode accept (Seq(Set('a'), Set('b'), Set('b'), Set('a'), Set('b'))))
  }


}
