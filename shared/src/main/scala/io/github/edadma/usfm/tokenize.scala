package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

abstract class Token:
  var pos: CharReader = uninitialized

  def setPos(r: CharReader): Token =
    pos = r
    this

case class Paragraph(name: String, num: Option[Int]) extends Token
case class Character(name: String)                   extends Token
case class Note(name: String)                        extends Token
case class End(name: String)                         extends Token
case class Text(s: String)                           extends Token
case object Space                                    extends Token
case object NoBreakSpace                             extends Token
case object LineBreak                                extends Token

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
val delimitedMarkers = Set("ior", "iqt", "rq", "ca", "va", "vp")
val characterMarkers = Set("v")

@tailrec
private def consume(r: CharReader, restrict: Boolean, buf: StringBuilder = new StringBuilder): (String, CharReader) =
  if r.ch.isWhitespace || (restrict && r.ch.isDigit) || r.ch == '\\' || r.ch == '*' || r.eoi then (buf.toString, r)
  else
    buf += r.ch
    consume(r.next, restrict, buf)

def tokenize(input: String): Seq[Token] =
  val buf = new ArrayBuffer[Token]

  @tailrec
  def tokenize(r: CharReader): Seq[Token] =
    r.ch match
      case CharReader.EOI => buf.toSeq
      case '~' =>
        buf += NoBreakSpace
        tokenize(r.next)
      case '/' if r.next.ch == '/' =>
        buf += LineBreak
        tokenize(r.next.next)
      case '\\' =>
        val plus =
          if r.ch == '+' then r.next
          else r
        val (marker, r1) = consume(plus, true)

        if marker.isEmpty then problem(r, "empty marker")

        if r1.ch == '*' then
          if !delimitedMarkers(marker) then problem(r, "invalid end marker")

          buf += End(marker).setPos(r)
          tokenize(r1.next.skipWhitespace)
        else
          if paragraphMarkers(marker) then
            val (number, r2) = if r.ch.isDigit then
              if !numberedMarkers(marker) then problem(r, "not a numbered marker")
              consume(r1, false)
            else
              (if numberedMarkers(marker) then "1" else "", r1)

            buf += Paragraph(marker, if number.nonEmpty then Some(number.toInt) else None).setPos(r)
            tokenize(r2.skipWhitespace)
          else if delimitedMarkers(marker) then
            buf += Character(marker).setPos(r)
            tokenize(r1.skipWhitespace)
          else if characterMarkers(marker) then
            buf += Character(marker).setPos(r)
            tokenize(r1.skipWhitespace)
          else problem(r, "invalid marker")
      case w if w.isWhitespace =>
        if buf.nonEmpty && buf.last != Space then
          buf += Space

        tokenize(r.next.skipWhitespace)
      case _ =>
        val (text, r1) = consume(r, false)

        buf += Text(text)
        tokenize(r1)
  end tokenize

  tokenize(CharReader.fromString(input))
end tokenize
