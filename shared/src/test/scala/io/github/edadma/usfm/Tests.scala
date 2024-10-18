package io.github.edadma.usfm

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers {

  "test" in {
    tokenize("asdf") shouldBe Seq(Text())
  }

}
