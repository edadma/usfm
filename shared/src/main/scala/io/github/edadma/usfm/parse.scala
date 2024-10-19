package io.github.edadma.usfm

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def dropSpace(t: LazyList[Token]): LazyList[Token] =
  t.lastOption match
    case Some(Space) => t.dropRight(1)
    case _           => t

def parse(toks: LazyList[Token]): LazyList[Elem] =
  def paragraphElem(e: ParagraphElem): Elem =
    e match
      case ParagraphElem("c", _, Seq(ch @ Text(chapter))) =>
        if !chapter.forall(_.isDigit) then problem(ch.pos, "expected chapter number")

        ChapterElem(chapter)
      case _ => e

  toks match
    case EOI() #:: _ => LazyList.empty
    case ParagraphStart(name, num) #:: tail if !pairedMarkers(name) =>
      tail.indexWhere(_.isInstanceOf[ParagraphToken]) match
        case -1 => paragraphElem(ParagraphElem(name, num, parseInline(dropSpace(tail)))) #:: LazyList.empty
        case idx =>
          val (p, rest) = tail.splitAt(idx)

          paragraphElem(ParagraphElem(name, num, parseInline(dropSpace(p)))) #:: parse(rest)

def parseInline(toks: LazyList[Token]): LazyList[Elem] =
  toks match
    case LazyList() => LazyList.empty
    case NoteStart(name) #:: tail if pairedMarkers(name) =>
      println(name)
      val (body, rest) = parseBody(tail)

      if !rest.headOption.contains(End(name)) then problem(rest.head.pos, s"expected '$name' end marker")

      NoteElem(name, body) #:: parseInline(rest.tail)
    case (t: ContentToken) #:: tail => t #:: parseInline(tail)
    case CharacterStart("v") #:: tail =>
      tail match
        case Text(verse) #:: rest if verse.forall(_.isDigit) =>
          if rest.head == Space then VerseElem(verse) #:: parseInline(rest.tail)
          else VerseElem(verse) #:: parseInline(rest)
        case t #:: _ => problem(t.pos, "verse number expected")

@tailrec
def parseBody(toks: LazyList[Token], body: ListBuffer[Elem] = new ListBuffer): (Seq[Elem], LazyList[Token]) =
  toks match
    case (_: End) #:: _ => (body.toSeq, toks)
    case h #:: t =>
      body += h
      parseBody(t, body)
