package io.github.edadma.usfm

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TokenizeTests extends AnyFreeSpec with Matchers:

  "text 1" in {
    tokenize("asdf") shouldBe Seq(Text("asdf"), EOI())
  }

  "text 2" in {
    tokenize("asdf zxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"), EOI())
  }

  "text 3" in {
    tokenize("asdf  zxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"), EOI())
  }

  "text 4" in {
    tokenize("asdf\nzxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"), EOI())
  }

  "paragraph markers 1" in {
    tokenize("""\p""") shouldBe Seq(ParagraphStart("p", None), EOI())
  }

  "paragraph markers 2" in {
    tokenize("""\p asdf""") shouldBe Seq(ParagraphStart("p", None), Text("asdf"), EOI())
  }

  "paragraph markers 3" in {
    a[RuntimeException] should be thrownBy tokenize("""\p1 asdf""")
  }

  "line break 1" in {
    tokenize("""//""") shouldBe Seq(LineBreak, EOI())
  }

  "line break with text" in {
    tokenize("""asdf//zxcv""") shouldBe Seq(Text("asdf"), LineBreak, Text("zxcv"), EOI())
  }

  "no-break space" in {
    tokenize("asdf~zxcv") shouldBe Seq(Text("asdf"), NoBreakSpace, Text("zxcv"), EOI())
  }

  "multiple newlines treated as space" in {
    tokenize("asdf\n\n\nzxcv") shouldBe Seq(Text("asdf"), Space, Text("zxcv"), EOI())
  }

  "empty marker" in {
    a[RuntimeException] should be thrownBy tokenize("""\ asdf""")
  }

  "invalid marker" in {
    a[RuntimeException] should be thrownBy tokenize("""\x""")
  }

//  "nested markers" in {
//    tokenize("""\bd bold \it italic \it* \bd* \bd more bold \bd*""") shouldBe Seq(
//      Character("bd"),
//      Text("bold"),
//      Space,
//      Character("it"),
//      Text("italic"),
//      Space,
//      End("it"),
//      Space,
//      End("bd"),
//      Space,
//      Character("bd"),
//      Text("more bold"),
//      Space,
//      End("bd"),
//    )
//  }

  "empty input" in {
    tokenize("") shouldBe Seq(EOI())
  }
