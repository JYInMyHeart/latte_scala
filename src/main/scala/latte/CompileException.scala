package latte

class CompileException(val message: String, val throwable: Throwable)
  extends Exception(message, throwable) {
  def this(throwable: Throwable) = this(null, throwable)

  def this(message: String) = this(message, null)

}

class SyntaxException(val msg: String, val lineCol: LineCol)
  extends CompileException(
    s"${if (msg == null) "syntax exception" else msg}${
      if (lineCol == LineCol.SYNTHETIC) ""
      else
        s" at (${lineCol.fileName},${lineCol.line},${lineCol.column})"
    }") {
  def this(lineCol: LineCol) =
    this(null, lineCol)

}

case class DuplicateVariableNameException(name: String,
                                          override val lineCol: LineCol)
  extends SyntaxException(name, lineCol)

case class UnexpectedTokenException(expected: String,
                                    got: String,
                                    override val lineCol: LineCol)
  extends SyntaxException(
    s"expecting $expected,but got $got",
    lineCol
  ) {
  def this(token: String, lineCol: LineCol) =
    this(token, null, lineCol)
}


case class LtBug(msg: String,
                 t: Throwable) extends Error(msg, t) {
  def this(t: Throwable) =
    this(null, t)

  def this(s: String) =
    this(s, null)

}