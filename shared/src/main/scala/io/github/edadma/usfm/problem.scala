package io.github.edadma.usfm

import io.github.edadma.char_reader.CharReader

def problem(pos: CharReader, error: String): Nothing =
  if (pos eq null)
    sys.error(error)
  else
    sys.error(s"${pos.line}, ${pos.col}: $error" + "\n" + pos.lineText)
