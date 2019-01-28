package latte

import java.io.{BufferedReader, StringReader}

import latte.Lexer._

case class Lexer(fileName: String,
                 reader: BufferedReader,
                 indent: Int) {
  def parse: ElementStartNode = {
    val args: Args = Args()
    args.fileName = fileName
    val elementStartNode = ElementStartNode(args, 0)
    args.startNodeStack.push(elementStartNode)
    parse(args)
    finalCheck(elementStartNode)
    elementStartNode
  }


  def redirectToStartNodeByIndent(args: Args, i: Int): Unit = {
    val startNode = args.startNodeStack.pop()

    if (startNode == null) throw new NoSuchElementException()
    if (startNode.indent == i) {
      if (startNode.hasNext)
        throw new Exception(s"Unexpected Token ${startNode.next}")
      args.previous = startNode
    }
    else {
      if (startNode.indent < i || args.startNodeStack.empty())
        throw new NoSuchElementException(s"position = " +
          s"${args.currentLine}:${args.currentCol},indent=$i")
      redirectToStartNodeByIndent(args, i)
    }
  }

  def createStartNode(args: Args): Unit = {
    val elementStartNode =
      ElementStartNode(args, args.startNodeStack.lastElement().indent + indent)
    args.previous = null
    args.startNodeStack.push(elementStartNode)
  }

  def parse(args: Args): Unit = {
    var line = reader.readLine()
    var rootIndent = -1
    while (line != null) {
      args.currentLine += 1

      line match {
        case l if l.startsWith("define") =>
          if (!line.equals("define")
            && SPLIT.contains(l.substring("define".length, "define".length + 1))) {
            args.currentCol += 1

            val originalString = l
            line = l.substring("define".length)
            args.currentCol += "define".length

            val las1 = getStringFroProcessing(line, args, originalString, "define")
            line = las1.line
            val target = las1.str.substring(1, las1.str.length - 1)
            target match {
              case x if x.isEmpty =>
                throw new Exception(s"define <target> length cannot be 0 at ${las1.lineCol}")
              case x if x.contains(ESCAPE) =>
                throw new Exception(s"define <target> cannot contain escape char at ${las1.lineCol}")
              case _ if !line.trim.startsWith("as") =>
                throw new Exception(
                  s"""illegal define command
                     |(there should be an `as` between <target> and <replacement>)
                     | at ${args.generateLineCol}""".stripMargin)
              case _ =>
            }
            val asPos = line.indexOf("as")
            line = line.substring(asPos + 2)
            line match {
              case x if x.isEmpty =>
                throw new Exception(s"illegal define command $originalString at ${args.generateLineCol}")
              case x if !x.contains(x.charAt(0).toString) =>
                throw new Exception(
                  s"""illegal define command
                     |(there should be an `as` between <target> and <replacement>)
                     | at ${args.generateLineCol}""".stripMargin)
              case _ =>
            }

            args.currentCol += asPos + 2
            val las2 = getStringFroProcessing(line, args, originalString, "define")
            line = las2.line
            val replacement = las2.str.substring(1, las2.str.length - 1)

            if (replacement.contains(ESCAPE))
              throw new Exception(s"define <replacement> cannot contain escape char at ${las1.lineCol}")
            if (!line.trim.isEmpty)
              throw new Exception(
                s"""illegal define command
                   |(there should not be any characters between after <replacement>)
                   | at ${args.generateLineCol}""".stripMargin)

            args.defined += target -> replacement
            line = reader.readLine()
            args.currentCol = 0
          }
        case x if x.startsWith("undef") =>
          if (x != "undef"
            && SPLIT.contains(x.substring("undef".length, "undef".length + 1))) {
            args.currentCol += 1
            val lineStart: LineCol = args.generateLineCol
            val originalString = x
            line = x.substring("undef".length)
            args.currentCol += "undef".length
            val las1 = getStringFroProcessing(line, args, originalString, "undef")
            line = las1.line
            val target = las1.str.substring(1, las1.str.length - 1)
            target match {
              case l if l.isEmpty =>
                throw new Exception(s"undef <target> length cannot be 0 at ${las1.lineCol}")
              case l if l.contains(ESCAPE) =>
                throw new Exception(s"undef <target> cannot contain escape char at ${las1.lineCol}")
              case _ =>
                if (line.trim.nonEmpty)
                  throw new Exception(
                    s"""illegal undef command
                       |(there should be any characters after <target>)
                       | at ${args.generateLineCol}""".stripMargin)
                if (!args.defined.contains(target))
                  throw new Exception(s"$target is not defined at $lineStart")
            }
            args.defined -= target
            line = reader.readLine()
            args.currentCol = 0
          }
        case _ =>
      }
      var anotherLine = line
      if (anotherLine.trim.startsWith(COMMENT)) {
        anotherLine = reader.readLine()
      } else {
        val COMMENT_index = line.indexOf(COMMENT)
        if (COMMENT_index != -1) {
          var pre = line.substring(0, COMMENT_index)
          val post = line.substring(COMMENT_index)
          for (entry <- args.defined)
            pre = pre.replace(entry._1, entry._2)
          line = pre + post
        } else {
          for (entry <- args.defined)
            line = line.replace(entry._1, entry._2)
        }
        var spaces = 0
        spaces = line.takeWhile(_ == ' ').length


        if (rootIndent == -1) {
          rootIndent = spaces
          spaces = 0
        } else
          spaces -= rootIndent

        if (spaces % indent != 0)
          throw new Exception("indent error")

        line = line.trim
        args.currentCol = spaces + 1 + rootIndent

        if (line.isEmpty)
          line = reader.readLine()
        else {
          if (args.startNodeStack.lastElement().indent != spaces) {
            if (args.startNodeStack.lastElement().indent > spaces)
              redirectToStartNodeByIndent(args, spaces + indent)
            else if (args.startNodeStack.lastElement().indent == spaces - indent)
              createStartNode(args)
            else
              throw new Exception("indent error")
          }
          parse(line, args)

          if (args.previous.isInstanceOf[Element]) {
            args.previous = new EndingNode(EndingNode.WEAK, args)
          }
          line = reader.readLine()
        }

      }
    }
  }

  def parse(l: String, args: Args): Unit = {
    var line = l
    if (line.isEmpty) return
    var minIndex = line.length
    var token: String = null
    for (s <- SPLIT) {
      if (line.contains(s)) {
        val index = line.indexOf(s)
        if (index != -1 && index < minIndex) {
          minIndex = index
          token = s
        }
      }
    }
    if (token == null)
      args.previous = Element(line, args)
    else {
      val copyOfLine = line
      val str = line.substring(0, minIndex)
      str match {
        case s if s.nonEmpty =>
          args.previous = Element(s, args)
          args.currentCol += str.length
        case _ =>
      }
      token match {
        case t if LAYER.contains(t) =>
          args.previous = Element(token, args)
          createStartNode(args)
        case t if SPLITS.contains(t) =>
          if (!NO_RECORD.contains(t))
            args.previous = Element(t, args)
        case t if STRING.contains(t) =>
          var lastIndex = minIndex
          var enable = true
          while (enable) {
            val index = line.indexOf(token, lastIndex + 1)
            if (line.length <= 1 || index == -1)
              throw new Exception("end of string not found")
            val c = line.charAt(index - 1)
            if (ESCAPE != c.toString) {
              val s = line.substring(minIndex, index + 1)
              args.previous = Element(s, args)
              args.currentCol += (index - minIndex + 1 - t.length)
              line = line.substring(index + 1)
              token = s
              enable = false
            } else {
              lastIndex = index
            }
          }
        case ENDING =>
          if (args.previous.isInstanceOf[Element])
            args.previous = EndingNode(EndingNode.STRONG, args)
        case COMMENT =>
          line = ""
        case t if PAIR.contains(t) =>
          args.previous = Element(token, args)
          createStartNode(args)
          args.pairEntryStack.push(PairEntry(token, args.startNodeStack.lastElement()))
        case t if PAIR.values.exists(_ == t) =>
          val entry = args.pairEntryStack.pop()
          val start = entry.key
          if (token != PAIR(start))
            throw new Exception(s"${PAIR(start)}$token${args.generateLineCol}")
          val startNode = entry.startNode
          if (startNode.hasNext)
            throw new Exception(s"null${startNode.next.toString()}${args.generateLineCol}")
          if (args.startNodeStack.lastElement().indent >= startNode.indent)
            redirectToStartNodeByIndent(args, startNode.indent)
          else if (args.startNodeStack.lastElement().indent == startNode.indent - indent)
            args.previous = startNode
          else
            throw new Exception(s"indentation of $token should >= " +
              s"$start 's indent or equal to 's indent - $indent at ${args.generateLineCol}")
          args.previous = Element(token, args)
        case _ =>
          throw new Exception(s"$token ${args.generateLineCol}")
      }
      args.currentCol += token.length
      if (copyOfLine == line)
        line = line.substring(minIndex + token.length)
      parse(line, args)

    }
  }


  private def getStringFroProcessing(line: String,
                                     args: Args,
                                     originalString: String,
                                     command: String): LineAndString = {
    val str = line.trim
    if (str.isEmpty)
      throw new Exception(s"illegal $command command $originalString at ${args.generateLineCol}")
    var token = str.charAt(0).toString
    if (!STRING.contains(token))
      throw new Exception(s"illegal $command command $originalString at ${args.generateLineCol}")
    args.currentCol += line.indexOf(token)
    var newLine = str
    var lastIndex = 0
    val lineCol = args.generateLineCol
    var enable = true
    while (enable) {
      val index = newLine.indexOf(token, lastIndex + 1)
      if (newLine.length <= 1 || index == -1)
        throw new Exception(s"end of string not found at $lineCol")
      val c = newLine.charAt(index - 1)
      if (ESCAPE != c.toString) {
        val s = newLine.substring(0, index + 1)
        args.currentCol += (index + token.length)
        newLine = newLine.substring(index + 1)
        token = s
        enable = false
      } else
        lastIndex = index
    }
    val las = LineAndString()
    las.line = newLine
    las.str = token
    las.lineCol = lineCol
    las
  }

  private def finalCheck(root: ElementStartNode): Unit = {
    if (root.hasLinkedNode) {
      var n = root.linkNode
      while (n != null) {
        n match {
          case x: ElementStartNode =>
            finalCheck(x)
          case x: EndingNode if !x.hasNext || !x.next.isInstanceOf[Element] =>
            if (n.hasPrevious)
              n.previous.next = x.next
            if (n.hasNext)
              n.next.previous = n.previous
          case x: Element =>
            x.checkWhetherIsValidName()
            if (x.content == "."
              && n.hasNext
              && n.hasPrevious
              && n.previous.isInstanceOf[Element]
              && n.next.isInstanceOf[Element]
              && CompilerUtil.isNumber(n.previous.asInstanceOf[Element].content)
              && CompilerUtil.isNumber(n.next.asInstanceOf[Element].content)
              && !n.previous.asInstanceOf[Element].content.contains(".")
              && !n.next.asInstanceOf[Element].content.contains(".")
            ) {
              val pre = x.previous.asInstanceOf[Element]
              val nextElement = x.next.asInstanceOf[Element]
              val s = pre.content + "." + nextElement.content
              val element = Element(s, Args())
              element.lineCol = pre.lineCol
              element.previous = pre.previous
              element.next = nextElement.next
              if (element.hasPrevious)
                element.previous.next = element
              else
                root.linkNode = element
              if (element.hasNext)
                element.next.previous = element
            }
          case _ =>
        }
        n = n.next
      }
      n = root.linkNode
      while (n != null) {
        n match {
          case x: ElementStartNode if x.hasNext =>
            val nextElement = x.next
            val args = Args()
            args.previous = n
            args.currentLine = x.lineCol.line
            args.currentCol = x.lineCol.column
            val endingNode = EndingNode(EndingNode.WEAK, args)
            endingNode.next = nextElement
            nextElement.previous = endingNode
          case _ =>
        }
        n = n.next
      }
    } else {
      if (root.hasPrevious)
        root.previous.next = root.next
      if (root.hasNext)
        root.next.previous = root.previous
    }
  }
}

private case class LineAndString() {
  var str: String = _
  var line: String = _
  var lineCol: LineCol = _
}


object Lexer {
  val LAYER: Set[String] = Set("#>", "#", "->")
  val STRING: Set[String] = Set("\"", "'", "`")
  val NO_RECORD: Set[String] = Set(" ")
  val ESCAPE: String = "\\"
  val ENDING = ","
  val COMMENT = ";"
  var PAIR: Map[String, String] = Map(
    "(" -> ")",
    "{" -> "}",
    "[" -> "]",
  )
  val SPLITS: Set[String] = Set(
    ".",
    ":",
    "::",
    "=", "+=", "-=", "*=", "/=", "%=",
    ">>", "<<", ">>>",
    "&", "^", "|", "~",
    "^^",
    "!", "&&", "||",
    "!=", "==", "!==", "===",
    "<", ">", "<=", ">=",
    "+", "-", "*", "/", "%",
    "++", "--",
    "@",
    "=:=", "!:=",
    "..", ".:",
    "..."
  ) ++ NO_RECORD
  var SPLIT: List[String] = STRING.toList ++
    (LAYER ++ SPLITS ++
      Set(ENDING, COMMENT) ++
      PAIR.keySet ++
      PAIR.values.toSet
      ).toList.sortBy(_.length).reverse
}

object Main extends App {
  val processor = new Lexer("test", new BufferedReader(new StringReader("" + "#> package::name::*\n" + "    package::name::Test")), 4)
  val root = processor.parse
  println(root.linkNode)
}
