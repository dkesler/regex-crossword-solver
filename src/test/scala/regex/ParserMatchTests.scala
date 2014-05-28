package regex

import org.scalatest.FunSuite

class ParserMatchTests extends FunSuite {

  def doTest(pattern: String, matching: List[List[Set[Char]]], notMatching: List[List[Set[Char]]]) {
    val node = Parser.compile(pattern)
    matching.map { case x => assert(node.accept(x), s"$pattern should have matched $x") }
    notMatching.map { case x => assert(!node.accept(x), s"$pattern should not have matched $x") }
  }
  def str(string: String): List[Set[Char]] = {
    string.toList.map { Set(_) }
  }
  test("a") {
    doTest(
      "a",
      List(str("a"), str("abc"), str("bca"), List(Set('a', 'b')), List(Set('b'), Set('c', 'a'))),
      List(str("b"), str("bc"), List(Set('b')))
    )
  }

  test("^a+$") {
    doTest(
      "^a+$",
      List(str("a"), str("aa"), List(Set('a', 'b')), List(Set('b', 'a'), Set('c', 'a'))),
      List(str("b"), str("ab"), List(Set('b')), List(Set('a'), Set('b', 'c')))
    )
  }

  test("^a*$") {
    doTest(
      "^a*$",
      List(str("a"), str("aa"), str(""), List(Set('a', 'b'))),
      List(str("ab"), List(Set('a', 'b'), Set('c')))
    )
  }

  test("^a{3}$") {
    doTest(
      "^a{3}$",
      List(str("aaa"), List(Set('a', 'b'), Set('a', 'c'), Set('a', 'd'))),
      List(str("aa"), str("aaaa"), str("aaba"), List(Set('a', 'b'), Set('a', 'c'), Set('d')))
    )
  }

  test("^a{3,}$") {
    doTest(
      "^a{3,}$",
      List(str("aaa"), str("aaaa"), List(Set('a', 'b'), Set('a', 'c'), Set('a', 'd'))),
      List(str("aa"), str("aba"))
    )
  }

  test("^a{3,5}$") {
    doTest(
      "^a{3,5}$",
      List(str("aaa"), str("aaaa"), str("aaaaa"), List(Set('a', 'b'), Set('a', 'c'), Set('a', 'd'))),
      List(str("aa"), str("aaaaaa"))
    )
  }

  test("^a[bcd]$") {
    doTest(
      "^a[bcd]$",
      List(str("ab"), str("ac"), List(Set('a', 'b'), Set('b', 'd', 'a'))),
      List(str("bc"), str("acd"), List(Set('a'), Set('e', 'f', 'a')))
    )
  }

  test("^a[bcd]+$") {
    doTest(
      "^a[bcd]+$",
      List(str("ab"), str("ac"), str("acdbdb"), List(Set('a', 'b'), Set('b', 'd', 'a'))),
      List(str("bc"), List(Set('a'), Set('e', 'f', 'a')))
    )
  }

  test("^a*[bcd]$") {
    doTest(
      "^a*[bcd]$",
      List(str("b"), str("aaac"), str("ad"), List(Set('a', 'b'), Set('b', 'd', 'a')), List(Set('b', 'q'))),
      List(str("bc"), List(Set('a'), Set('e', 'f', 'a')))
    )
  }

  test("^a[^bcd]$") {
    doTest(
      "^a[^bcd]$",
      List(str("aq"), List(Set('a'), Set('b', 'q'))),
      List(str("ab"), List(Set('a'), Set('b', 'c', 'd')))
    )
  }

  test("^(foo)$") {
    doTest(
      "^(foo)$",
      List(str("foo"), List(Set('f', 'g'), Set('o'), Set('q', 'o'))),
      List(str(""), str("foo1"), List(Set('f'), Set('o'), Set('q', 'g')))
    )
  }

  test("^(foo)+$") {
    doTest(
      "^(foo)+$",
      str("foo") :: str("foofoo") :: List(Set('f'), Set('o'), Set('o', 'p')) :: Nil,
      str("") :: str("foobarfoo") :: str("fooo") :: List(Set('f'), Set('p', 'q'), Set('o')) :: Nil
    )
  }


  test("(foo+)") {
    doTest(
      "^(foo+)$",
      str("foo") :: str("foooo") :: List(Set('f'), Set('o'), Set('o', 'p'), Set('o')) :: Nil,
      str("") :: str("foofoo") :: List(Set('f'), Set('p', 'q'), Set('o')) :: Nil
    )
  }

  test("^(fo(bar)o)+\\1\\2$") {
    doTest(
      "^(fo(bar)o)+\\1\\2$",
      str("fobarofobarobar") :: Nil,
      str("") :: Nil
    )
  }

  test("^(.+)\\1$") {
    doTest(
      "^(.+)\\1$",
      str("aa") :: str("foofoo") :: List(Set('f'), Set('o'), Set('f', 'g'), Set('q', 'o')) :: Nil,
      str("ab") :: str("foo") :: List(Set('f'), Set('o', 'p'), Set('f'), Set('q')) :: Nil
    )
  }

  test("^(.)+\\1$") {
    doTest(
      "^(.)+\\1$",
      str("aa") :: str("aaa") :: str("abb")  :: List(Set('a'), Set('b', 'q'), Set('q')) :: Nil,
      str("aab") :: List(Set('a', 'b'), Set('c', 'd'), Set('a', 'b')) :: Nil
    )
  }

  test("^(.+)+\\1$") {
    doTest(
      "^(.+)+\\1$",
      str("aa") :: str("aaa") :: str("abab") :: str("ababab") :: str("abapap") :: List(Set('a'), Set('b', 'q'), Set('a'), Set('b', 'p')) :: List(Set('a'), Set('b', 'q'), Set('a'), Set('b', 'p'), Set('a'), Set('q', 'p')) :: Nil,
      str("aab") :: str("ababaq") :: Nil
    )
  }


  test("^(.*)\\1a$") {
    doTest(
      "^(.*)\\1a$",
      str("bbbbbba") :: str("a") :: List(Set('b', 'c'), Set('b', 'd'), Set('b'), Set('c', 'd'), Set('a')) :: Nil,
      str("bbba") :: Nil
    )
  }

  test("^(.)*\\1a$") {
    doTest(
      "^(.)*\\1a$",
      str("a") :: str("bba") :: str("bqppa") :: List(Set('b', 'c'), Set('d', 'e'), Set('d', 'q'), Set('a', 'l')) :: Nil,
      str("bqpba") :: str("bcbca") :: Nil
    )
  }


  test("^a|b|c$") {
    doTest (
      "^a|b|c$",
      str("a") :: str("b") :: str("c") :: List(Set('a', 'b')) :: List(Set('c', 'd')) :: Nil,
      str("abc") :: List(Set('d', 'e')) :: Nil
    )
  }

  test("^a|b+|(c)$") {
    doTest(
      "^a|b+|(c)$",
      str("a") :: str("b") :: str("bbb") :: str("c") :: List(Set('a', 'b')) :: List(Set('a', 'b'), Set('b')) :: Nil,
      str("ab") :: str("abb") :: List(Set('a', 'b'), Set('b'), Set('c')) :: Nil
    )
  }

  test("^a|b+|(c|d)\\1$") {
    doTest(
      "^a|b+|(c|d)\\1$",
      str("a") :: str("bb") :: str("cc") :: str("dd") :: List(Set('c', 'd'), Set('c', 'd')) :: List(Set('c', 'd'), Set('c')) :: List(Set('c', 'e'), Set('c')) :: Nil,
      str("ab") :: str("cd") :: List(Set('c', 'e'), Set('d', 'e')) :: Nil
    )
  }

  test("^(a|(b))\\2$") {
    doTest(
    "^(a|(b))\\2$",
    str("a") :: str("bb") :: List(Set('a', 'b'), Set('b')) :: List(Set('a', 'b'), Set('a', 'b')) :: List(Set('a', 'b')) :: Nil,
      str("aa") :: str ("ab") :: List(Set('a', 'b'), Set('a')) :: Nil
    )
  }

  test("""^\.$""") {
    doTest(
      """^\.$""",
      str(".") :: List(Set('a', '.')) :: Nil,
      str("a") :: Nil
    )
  }

  test("^a?$") {
    doTest(
      "^a?$",
      str("a") :: str("") :: List(Set('a', 'b')) :: List() :: Nil,
      str("aa") :: Nil
    )
  }

  test("^\\d$") {
    doTest(
      "^\\d$",
      str("0") :: List(Set('1', 'a')) :: Nil,
      str("a") :: Nil
    )

  }

  test("^\\s$") {
    doTest(
      "^\\s$",
      str(" ") :: List(Set(' ', 'a')) :: Nil,
      str("a") :: str("0") :: Nil
    )
  }

  test("^\\w$") {
    doTest(
      "^\\w$",
      str("0") :: str("a") :: str("_") :: List(Set('1',' ')) :: Nil,
      str(" ") :: Nil
    )
  }

  test("^[\\d\\s]$") {
    doTest(
      "^[\\d\\s]$",
      str("0") :: str(" ") :: List(Set('1',' ')) :: Nil,
      str("a") :: Nil
    )
  }

  test("^[\\D\\S]$") {
    doTest(
      "^[\\D\\S]$",
      str("0") :: str("a") :: str(" ") :: str("_") :: Nil,
      Nil
    )
  }

  test("^[^\\d\\s]$") {
    doTest(
      "^[^\\d\\s]$",
      str("a") :: Nil,
      str("0") :: str(" ") :: Nil
    )
  }
}
