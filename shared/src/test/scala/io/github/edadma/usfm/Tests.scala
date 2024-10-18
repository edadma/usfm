package io.github.edadma.usfm

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers:

  "text 1" in {
    tokenize("asdf") shouldBe Seq(Text("asdf"))
  }

  "text 2" in {
    tokenize("asdf zxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"))
  }

  "text 3" in {
    tokenize("asdf  zxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"))
  }

  "text 4" in {
    tokenize("asdf\nzxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"))
  }
