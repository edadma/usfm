package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable.ArrayBuffer

abstract class Token:
  val pos: CharReader
case class Paragraph(name: String, pos: CharReader, num: Option[Int]) extends Token
case class Character(name: String, pos: CharReader)                   extends Token
case class Note(name: String, pos: CharReader)                        extends Token
case class End(name: String, pos: CharReader)                         extends Token
case class Text(s: String, pos: CharReader)                           extends Token

val paragraphMarkers =
  Set(
    "id",
    "usfm",
    "ide",
    "sts",
    "rem",
    "h",
    "toc",
    "toca",
    "imt",
    "is",
    "ip",
    "ipi",
    "im",
    "imi",
    "ipq",
    "imq",
    "ipr",
    "iq",
    "ib",
    "ili",
    "iot",
    "io",
    "iex",
    "imte",
    "ie",
    "mt",
    "mte",
    "ms",
    "mr",
    "s",
    "sr",
    "r",
    "d",
    "sp",
    "sd",
    "c",
    "cl",
    "cp",
    "cd",
    "p",
    "m",
    "po",
    "pr",
    "cls",
    "pmo",
    "pm",
    "pmc",
    "pmr",
    "pi",
    "mi",
    "nb",
    "pc",
    "ph",
    "b",
    "lit",
    "pb",
  )
val numberedMarkers =
  Set("toc", "toca", "imt", "is", "iq", "ili", "io", "imte", "mt", "mte", "ms", "s", "sd", "pi", "ph")
val characterMarkers =
  Set("ior", "iqt", "rq", "ca", "va", "vp")
val character1Markers =
  Set("v")
def tokenize(input: CharReader): Seq[Token] =
  val buf = new ArrayBuffer[Token]

  def tokenize(r: CharReader): Seq[Token] =
    r.ch match
      case CharReader.EOI          => buf.toSeq
      case '~'                     =>
      case '/' if r.next.ch == '/' =>
      case '\\'                    =>
      case w if w.isWhitespace     =>
      case _                       =>
  end tokenize

  tokenize(input)
end tokenize
