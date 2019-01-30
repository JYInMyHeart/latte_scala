package latte

import java.util

case class LineCol(fileName: String, line: Int, column: Int)

object LineCol {
  val SYNTHETIC: LineCol = LineCol(null, 0, 0)
}

case class PairEntry(key: String, startNode: ElementStartNode)

case class Args() {
  var fileName: String = _
  var previous: Node = _
  var currentLine: Int = _
  var currentCol: Int = _

  import util.Stack

  val startNodeStack: Stack[ElementStartNode] =
    new Stack[ElementStartNode]()
  val pairEntryStack: Stack[PairEntry] = new Stack[PairEntry]
  lazy val generateLineCol = new LineCol(fileName, currentLine, currentCol)
  var defined: Map[String, String] = Map()
}

abstract class Node() {
  var next: Node = _
  var previous: Node = _
  var lineCol: LineCol = _

  def this(args: Args) = {
    this()
    lineCol = LineCol(args.fileName, args.currentLine, args.currentCol)
    this.previous = args.previous
    if (hasPrevious) {
      assert(previous != null)
      previous.next = this
    } else if (!args.startNodeStack.isEmpty)
      args.startNodeStack.lastElement().linkNode = this
  }

  def hasNext: Boolean = next != null

  def hasPrevious: Boolean = previous != null

  def toString(indent: Int): String
}

case class EndingNode(nodeType: Int, arg: Args) extends Node(arg) {

  override def toString(indent: Int): String = {
    var sb = ""
    for (i <- 0 until indent)
      sb = sb.concat(" ")
    val msg = if (nodeType == EndingNode.STRONG) "STRONG" else "WEAK"
    sb = sb.concat("(").concat(msg).concat(")\n")
    if (hasNext)
      sb = sb.concat(next.toString(indent))
    sb
  }

  override def toString: String =
    if (nodeType == EndingNode.STRONG)
      ","
    else
      "NewLine"
}

object EndingNode {
  val STRONG = 0
  val WEAK = 1
}

case class Element(var content: String, arg: Args) extends Node(arg) {

  def checkWhetherIsValidName(): Unit = {
    if (CompilerUtil.isValidName(content))
      isValidName = true
    if (content.startsWith("`"))
      content = content.substring(1, content.length - 1)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass) return false
    val that: Element = obj.asInstanceOf[Element]
    content == that.content
  }

  override def toString(indent: Int): String = {
    var sb = ""
    for (i <- 0 until indent)
      sb = sb.concat(" ")
    sb = sb.concat(content).concat("\n")
    if (hasNext)
      sb = sb.concat(next.toString(indent))
    sb
  }

  override def toString: String = content

  var isValidName: Boolean = _
}

case class ElementStartNode(arg: Args, indent: Int) extends Node(arg) {
  var linkNode: Node = _

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass) return false
    val that: ElementStartNode = obj.asInstanceOf[ElementStartNode]
    linkNode == that.linkNode
  }

  override def toString: String = "NewLayer"

  def hasLinkedNode: Boolean = linkNode != null

  def toString(indent: Int): String = {
    var buffer: String = ""
    if (hasLinkedNode)
      buffer = buffer.concat(linkNode.toString(indent + 4))
    if (hasNext)
      buffer = buffer.concat(next.toString(indent))
    buffer
  }
}
