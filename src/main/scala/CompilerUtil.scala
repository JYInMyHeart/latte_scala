package latte

object CompilerUtil {

  val javaKeys: Set[String] = Set(
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "null"
  )

  val oneVarOperatorsPreWithoutCheckingExps: Set[String] = Set("!", "~")
  val oneVarOperatorsPreMustCheckExps: Set[String] = Set(
    "++",
    "--",
    "!",
    "~",
    "+",
    "-"
  )

  val modifiers: Set[String] = Set(
    "pub",
    "pro",
    "pri",
    "pkg",
    "abs",
    "val",
    "native",
    "sync",
    "transient",
    "volatile",
    "strictfp",
    "data"
  )

  val accessModifiers: Set[String] = Set(
    "pub",
    "pro",
    "pri",
    "pkg"
  )

  val keys: Set[String] = Set(
    "is",
    "bool",
    "yes",
    "no",
    "type",
    "as",
    "undefined"
  ) ++ modifiers ++ javaKeys

  val oneVarOperatorsPost: Set[String] = Set("++", "--")

  val binOpPriority: Array[Array[String]] = Array(
    Array("..", ".:"),
    Array("^^"),
    Array("*", "/", "%"),
    Array("+", "-"),
    Array("<<", ">>", ">>>"),
    Array(">", "<", ">=", "<="),
    Array("==", "!=", "===", "!==", "=:=", "!:=", "is", "not", "in"),
    Array("&"),
    Array("^"),
    Array("|"),
    Array("&&", "and"),
    Array("||", "or")
  )

  val NOT_METHOD_DEF = 0
  val METHOD_DEF_NORMAL = 1
  val METHOD_DEF_TYPE = 2
  val METHOD_DEF_EMPTY = 3
  val METHOD_DEF_ONE_STMT = 4

  val twoVarOperators: Set[String] =
    binOpPriority.foldLeft(List[String]())(_ ::: _.toList).toSet

  val primitives: Set[String] = Set(
    "int",
    "double",
    "float",
    "short",
    "long",
    "byte",
    "char",
    "bool"
  )

  def isNumber(str: String): Boolean =
    str.forall(Character.isDigit)

  def isValidNameStartChar(c: Char): Boolean =
    Character.isLetter(c) || c == '$' || c == '_'

  def isValidNameChar(c: Char): Boolean =
    isValidNameStartChar(c) || Character.isDigit(c)

  def isJavaValidName(str: String): Boolean = {
    str match {
      case x if x.isEmpty => false
      case x if javaKeys.contains(x) => false
      case x if isValidNameStartChar(x.charAt(0)) =>
        for (i <- 1 until x.length) {
          if (!isValidNameChar(x.charAt(i)))
            return false
        }
        true
      case _ => false
    }
  }

  def isValidName(str: String): Boolean = {
    if (str.startsWith("`") && str.endsWith("`"))
      return isJavaValidName(str.substring(1, str.length - 1))
    isJavaValidName(str) && !keys.contains(str)
  }

  def isBoolean(str: String): Boolean =
    str == "true" || str == "false"

  def isString(str: String): Boolean =
    (str.startsWith("\"") && str.endsWith("\"")) || (str.startsWith("\'") && str
      .endsWith("\'"))

  def isOneVariableOperatorPost(str: String): Boolean =
    oneVarOperatorsPost.contains(str)

  def isTwoVariableOperator(str: String): Boolean =
    twoVarOperators.contains(str)

  def getNextNode(element: Node): Node = {
    if (element == null) return null
    if (element.next.isInstanceOf[EndingNode])
      getNextNode(element.next)
    else
      element.next
  }

  def isLambda(elem: Node): Boolean = {
    elem match {
      case e: Element =>
        if (e.content == "(") {
          var n = getNextNode(e)
          n match {
            case _: ElementStartNode =>
              n = getNextNode(n)
            case _ =>
          }

          n match {
            case x: Element =>
              if (x.content == ")") {
                n = getNextNode(n)
                n match {
                  case y: Element if y.content == "->" =>
                    return true
                  case _ =>
                }
              }
            case _ =>
          }
        }
    }
    false
  }

  def isAssign(str: String): Boolean =
    str == "=" || str == "/=" || str == "*=" || str == "+=" || str == "-=" || str == "%="

  def expecting(token: String, previous: Node, got: Node): Unit = {
    got match {
      case null =>
        throw new Exception(s"unexpected end ${previous.lineCol}")
      case x if !x.isInstanceOf[Element] =>
        throw UnexpectedTokenException(token,x.getClass.getSimpleName,x.lineCol)
      case x:Element if !x.content.endsWith(token) =>
        throw UnexpectedTokenException(token,x.content,x.lineCol)
      case _ =>
    }
  }

  def isPackage(element: Element): Boolean = {
    if (isValidName(element.content) && element.hasNext) {
      element.next match {
        case n: Element =>
          if (n.content == "::" && n.hasNext) {
            n.next match {
              case nn: Element =>
                return isValidName(nn.content)
              case _ =>
                return false
            }
          }
        case _ =>
          return false
      }
    }
    false
  }

  def twoVarHigherOrEqual(a: String, b: String): Boolean = {
    var indexA = findTwoVarPriority(a)
    if (indexA == -1) {
      if (isValidName(a))
        indexA = binOpPriority.length
      else
        throw new IllegalArgumentException(
          a + " is not valid two variable operator")
    }

    var indexB = findTwoVarPriority(b)
    if (indexB == -1) {
      if (isValidName(b))
        indexB = binOpPriority.length
      else
        throw new IllegalArgumentException(
          b + " is not valid two variable operator")
    }
    indexA <= indexB
  }

  def findTwoVarPriority(s: String): Int = {
    for (i <- binOpPriority.indices)
      for (j <- binOpPriority(i))
        if (j == s)
          return i
    -1
  }

  def isPrimitive(s: String): Boolean =
    primitives.contains(s)

  def modifierIsCompatible(str: String, modifiers: Set[Modifier]): Boolean = {
    val isAccessMod = accessModifiers.contains(str)
    for (m <- modifiers)
      if (m.modifier == str
        || (isAccessMod && accessModifiers.contains(m.modifier))
        || (str == "val" && m.modifier == "abs"
        || (str == "abs" && m.modifier == "val")))
        return false

    true
  }

  def isModifier(str: String) =
    modifiers.contains(str)

  def checkMethodDef(elem: Element): Int = {
    val content = elem.content
    if (isValidName(content)) {
      var nodeAfterRightPar: Node = null
      val n1 = getNextNode(elem)
      n1 match {
        case n: Element => {
          val p = n.content
          if (p == "(") {
            val n2 = getNextNode(n1)
            n2 match {
              case nn2: ElementStartNode => {
                val n3 = getNextNode(n2)
                n3 match {
                  case e: Element => {
                    if (e.content == ")") {
                      nodeAfterRightPar = getNextNode(n3)
                    }
                  }
                  case _ =>
                }
              }
              case nn2: Element => {
                if (nn2.content == ")") {
                  nodeAfterRightPar = getNextNode(n2)
                }
              }
              case _ =>
            }
          }
        }
        case _ =>
      }
      if (nodeAfterRightPar != null) {
        nodeAfterRightPar match {
          case n: ElementStartNode => {
            return METHOD_DEF_NORMAL
          }
          case n: Element => {
            val s = n.content
            s match {
              case ":" => {
                return METHOD_DEF_TYPE
              }
              case "=" => {
                val nn = getNextNode(nodeAfterRightPar)
                nn match {
                  case e: Element => {
                    if (e.content == "...") {
                      return METHOD_DEF_EMPTY
                    } else {
                      return METHOD_DEF_ONE_STMT
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    NOT_METHOD_DEF
  }
}
