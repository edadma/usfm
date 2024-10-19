package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import pprint.pprintln

@main def run(): Unit =
  val input =
    """
      |\id MRK - Berean Study Bible
      |\h Mark
      |\toc1 Mark
      |\mt1 Mark
      |\c 1
      |\s1 The Mission of John the Baptist
      |\r (Isaiah 40:1–5; Matthew 3:1–17; Luke 3:1–22; John 1:19–34)
      |\b
      |\m
      |\v 1 This is the beginning of the gospel of Jesus Christ, the Son of God.\f + \fr 1:1 \ft SBL and WH the beginning of the gospel of Jesus Christ.\f*
      |""".stripMargin
  val toks = tokenize(CharReader.fromFile("bsb_usfm/42MRKBSB.usfm"))
  val doc  = parse(toks)

  pprintln(doc)
