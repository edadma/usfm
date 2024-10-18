package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.uninitialized

abstract class Token:
  var pos: CharReader = uninitialized

  def setPos(r: CharReader): Token =
    pos = r
    this

case class Paragraph(name: String, num: Option[Int]) extends Token
case class Character(name: String)                   extends Token
case class Attributes(attr: Map[String, String])     extends Token
case class Note(name: String)                        extends Token
case class End(name: String)                         extends Token
case class Text(s: String)                           extends Token
case object Space                                    extends Token
case object NoBreakSpace                             extends Token
case object LineBreak                                extends Token
case object EOI                                      extends Token

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
val pairedMarkers = Set(
  "ior",
  "iqt",
  "rq",
  "ca",
  "va",
  "vp",
  "add",
  "bk",
  "dc",
  "k",
  "nd",
  "ord",
  "pn",
  "png",
  "addpn",
  "qt",
  "sig",
  "sls",
  "tl",
  "wj",
  "em",
  "bd",
  "it",
  "bdit",
  "no",
  "sc",
  "sup",
)
val characterMarkers = Set(
  "v",
  "add",
  "bk",
  "dc",
  "k",
  "nd",
  "ord",
  "pn",
  "png",
  "addpn",
  "qt",
  "sig",
  "sls",
  "tl",
  "wj",
  "em",
  "bd",
  "it",
  "bdit",
  "no",
  "sc",
  "sup",
)
val delimiters = Set('\\', '/', '~', '*', '|')

@tailrec
private def consume(r: CharReader, restrict: Boolean, buf: StringBuilder = new StringBuilder): (String, CharReader) =
  if r.ch.isWhitespace || (restrict && r.ch.isDigit) || delimiters(r.ch) || r.eoi
  then (buf.toString, r)
  else
    buf += r.ch
    consume(r.next, restrict, buf)

@tailrec
private def consumeUpTo(r: CharReader, delim: Char, buf: StringBuilder = new StringBuilder): (String, CharReader) =
  if r.ch == delim then
    (buf.toString, r.next.skipWhitespace)
  else if r.eoi then problem(r, "unexpected end of input in attribute")
  else
    buf += r.ch
    consumeUpTo(r.next, delim, buf)

def tokenize(input: String): LazyList[Token] = tokenize(CharReader.fromString(input))

def tokenize(input: CharReader): LazyList[Token] =
  def tokenize(r: CharReader): LazyList[Token] =
    r.ch match
      case CharReader.EOI          => LazyList(EOI)
      case '~'                     => NoBreakSpace #:: tokenize(r.next)
      case '/' if r.next.ch == '/' => LineBreak #:: tokenize(r.next.next)
      case '\\' =>
        val plus =
          if r.next.ch == '+' then r.next.next
          else r.next
        val (marker, r1) = consume(plus, true)

        if marker.isEmpty then problem(plus, "empty marker")

        if r1.ch == '*' then
          if !pairedMarkers(marker) then problem(plus, "invalid end marker")

          End(marker).setPos(plus) #:: tokenize(r1.next.skipWhitespace)
        else
          if paragraphMarkers(marker) then
            val (number, r2) = if r1.ch.isDigit then
              if !numberedMarkers(marker) then problem(plus, "not a numbered marker")
              consume(r1, false)
            else
              (if numberedMarkers(marker) then "1" else "", r1)

            Paragraph(marker, if number.nonEmpty then Some(number.toInt) else None).setPos(plus) #:: tokenize(
              r2.skipWhitespace,
            )
          else if pairedMarkers(marker) then Character(marker).setPos(plus) #:: tokenize(r1.skipWhitespace)
          else if characterMarkers(marker) then Character(marker).setPos(plus) #:: tokenize(r1.skipWhitespace)
          else problem(plus, "invalid marker")
      case w if w.isWhitespace => Space #:: tokenize(r.next.skipWhitespace)
      case '|' =>
        val map = new mutable.HashMap[String, String]

        @tailrec
        def attribute(r: CharReader): CharReader =
          val r1        = r.skipWhitespace
          val (key, r2) = consumeUpTo(r1, '=')
          val r3        = r2.skipWhitespace

          if r3.ch != '"' then problem(r3, "expected attribute value")

          val (value, r4) = consumeUpTo(r3.next, '"')
          val r5          = r4.skipWhitespace

          map += (key -> value)

          if r5.ch.isLetter then attribute(r5)
          else r5
        end attribute

        val r1 = attribute(r)

        Attributes(map.toMap) #:: tokenize(r1)
      case _ =>
        val (text, r1) = consume(r, false)

        Text(text) #:: tokenize(r1)
  end tokenize

  tokenize(input)
end tokenize
