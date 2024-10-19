package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

import scala.compiletime.uninitialized

abstract class Elem

case class ParagraphElem(name: String, num: Option[Int], body: Seq[Elem]) extends Elem
case class CharacterElem(name: String, body: Seq[Elem])                   extends Elem
case class NoteElem(name: String, body: Seq[Elem])                        extends Elem
case class ChapterElem(chapter: String)                                   extends Elem
case class VerseElem(verse: String)                                       extends Elem

abstract class Token extends Elem:
  var pos: CharReader = uninitialized

  def setPos(r: CharReader): Token =
    pos = r
    this

trait ContentToken   extends Token
trait ParagraphToken extends Token

case class ParagraphStart(name: String, num: Option[Int]) extends ParagraphToken
case class CharacterStart(name: String)                   extends Token
case class Attributes(attr: Map[String, String])          extends Token
case class NoteStart(name: String)                        extends Token
case class End(name: String)                              extends Token
case class Text(s: String)                                extends ContentToken
case object Space                                         extends ContentToken
case object NoBreakSpace                                  extends ContentToken
case object LineBreak                                     extends Token
case class EOI()                                          extends ParagraphToken
