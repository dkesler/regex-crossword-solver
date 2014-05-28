package regex

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserParseTests extends FunSuite {
  def parseTest(pattern: String) = {
    assert(Parser.parse(pattern).toString === pattern)
  }
  test("a") {
    parseTest("a")
  }

  test("a+") {
    parseTest("a+")
  }

  test("a*") {
    parseTest("a*")
  }

  test("a{3}") {
    parseTest("a{3}")
  }

  test("a{3,}") {
    parseTest("a{3,}")
  }

  test("a{3,5}") {
    parseTest("a{3,5}")
  }

  test("a[bcd]") {
    parseTest("a[bcd]")
  }

  test("a[bcd]+") {
    parseTest("a[bcd]+")
  }

  test("a*[bcd]") {
    parseTest("a*[bcd]")
  }

  test("a[bcd]e") {
    parseTest("a[bcd]e")
  }

  test("a[bcd]e+") {
    parseTest("a[bcd]e+")
  }

  test("a[^bcd]") {
    parseTest("a[^bcd]")
  }

  test("[^bcd][def]") {
    parseTest("[^bcd][def]")
  }

  test("(foo)") {
    parseTest("(foo)")
  }

  test("(foo)+") {
    parseTest("(foo)+")
  }

  test("a(foo)+") {
    parseTest("a(foo)+")
  }

  test("(foo)+b") {
    parseTest("(foo)+b")
  }

  test("(fo(bar)o)+") {
    parseTest("(fo(bar)o)+")
  }

  test("a|b|c") {
    parseTest("a|b|c")
  }

  test("a|b+|(c)") {
    parseTest("a|b+|(c)")
  }

  test("a|b+|(c|d)") {
    parseTest("a|b+|(c|d)")
  }

  test("""\.""") {
    parseTest("""\.""")
  }

  test("\\1") {
    parseTest("\\1")
  }

  test("\\13q") {
    parseTest("\\13q")
  }

  test("\\2+") {
    parseTest("\\2+")
  }

  test("^$") {
    parseTest("^$")
  }

  test("^po$") {
    parseTest("^po$")
  }

  test("^he||ll|o+(foo(bar+)[qz\\+]{3,})\\1\\2+$") {
    parseTest("^he||ll|o+(foo(bar+)[qz\\+]{3,})\\1\\2+$")
  }
}
