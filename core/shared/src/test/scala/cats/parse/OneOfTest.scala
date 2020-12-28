import cats.parse.{Parser => P}

class OneOfTest extends munit.FunSuite {

  def simplifyOrRule(rule: String): String = {
    // (a|b)a --> aa|ba
    // a(a|b) --> aa|ab
    //  (a|b) -->  a|b

    val term = P.charsWhile(_.isLetter)

    val orTerm = P.repSep(term, 2, P.char('|'))
    val bracedOrTerm = orTerm.between(P.char('('), P.char(')'))

    val wordRightOfBraced = (bracedOrTerm ~ term).map { case (orTerms, word) =>
      orTerms.map(or => or + word).toList.mkString("|")
    }
    val wordLeftOfBraced = (term ~ bracedOrTerm).map { case (word, orTerms) =>
      orTerms.map(or => word + or).toList.mkString("|")
    }
    val bracedOrTermWithoutBraces = bracedOrTerm.map { terms => terms.toList.mkString("|") }

    val parserList = wordRightOfBraced :: wordLeftOfBraced :: bracedOrTermWithoutBraces :: Nil
    val parser = P.oneOf(parserList)

    val results = parserList.map(p => p.parse(rule))
    results.map(_.isRight).zipWithIndex.foreach { case (isRight, idx) =>
      println(s"parserResult #$idx: $isRight")
    }
    //println("result of each parser")
    //results.foreach(println)

    val result = parser.parse(rule)

    result.fold(error => throw new RuntimeException(error.toString), _._2)
  }

  test("rule simplififaction - braced with terms on each side works") {

    assertEquals(simplifyOrRule("(a|b)a"), "aa|ba")
    assertEquals(simplifyOrRule("a(a|b)"), "aa|ab")
  }
  test("rule simplififaction - remove braces from simple or-term doesn't work") {

    // results of individual parsers
    // parserResult #0: false
    // parserResult #1: false
    // parserResult #2: true

    // oneOf with the list of these parsers fails.

    assertEquals(simplifyOrRule("(a|b)"), "a|b")

  }

}
