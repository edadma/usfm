package io.github.edadma.usfm

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParseTests extends AnyFreeSpec with Matchers:

  def parseTest(s: String): Seq[Elem] = parse(tokenize(s))

  "id" in {
    parseTest(
      """
        |\id MRK - Berean Study Bible
        |""".stripMargin,
    ) shouldBe
      Seq(
        ParagraphElem(
          name = "id",
          num = None,
          body = LazyList(
            Text(s = "MRK"),
            Space,
            Text(s = "-"),
            Space,
            Text(s = "Berean"),
            Space,
            Text(s = "Study"),
            Space,
            Text(s = "Bible"),
          ),
        ),
      )
  }
