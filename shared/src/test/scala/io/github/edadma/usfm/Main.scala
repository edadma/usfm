package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import pprint.pprintln

@main def run(): Unit =
  val toks = tokenize(CharReader.fromFile("bsb_usfm/01GENBSB.usfm"))

  pprintln(toks.length)
