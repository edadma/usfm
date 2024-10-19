package io.github.edadma.usfm

def parse(toks: LazyList[Token]): LazyList[Elem] =
  toks match
    case end if end == EOI => LazyList.empty
    case CharacterStart(name) #:: tail if !pairedMarkers(name) =>
      tail.indexWhere(_.isInstanceOf[CharacterStart]) match
        case -1 => CharacterElem(name, tail) #:: LazyList.empty
        case idx =>
          val (c, rest) = tail.splitAt(idx)

          CharacterElem(name, c) #:: parse(rest)
