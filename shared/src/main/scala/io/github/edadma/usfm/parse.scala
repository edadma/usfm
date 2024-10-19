package io.github.edadma.usfm

def dropSpace(t: LazyList[Elem]): LazyList[Elem] =
  t.lastOption match
    case Some(Space) => t.dropRight(1)
    case _           => t

def parse(toks: LazyList[Token]): LazyList[Elem] =
  toks match
    case end if end == EOI => LazyList.empty
    case ParagraphStart(name, num) #:: tail if !pairedMarkers(name) =>
      tail.indexWhere(_.isInstanceOf[ParagraphStart]) match
        case -1 => ParagraphElem(name, num, dropSpace(tail)) #:: LazyList.empty
        case idx =>
          val (p, rest) = tail.splitAt(idx)

          ParagraphElem(name, num, dropSpace(p)) #:: parse(rest)
    case NoteStart(name) #:: tail if pairedMarkers(name) =>

    case e #:: tail => e #:: parse(tail)
