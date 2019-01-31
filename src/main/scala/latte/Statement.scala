package latte

import scala.collection.mutable.ListBuffer

trait Statement {
  def lineCol: LineCol
}

/**
  * Expression
  *
  */
trait Expression extends Statement

/**
  * Access
  *
  */
case class Access(expression: Expression, name: String, lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    val result =
      if (expression != null)
        expression.hashCode
      else
        0
    31 * result + (if (name == null) 0 else name.hashCode)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    expression == obj.asInstanceOf[Access].expression
  }

  override def toString: String = s"(${if (expression == null) "" else expression}.$name)"
}

/**
  * Annotation
  *
  */
case class Anno(anno: Access, args: List[Assignment], lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = anno.hashCode()
    result = 31 * result + args.hashCode
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Anno(at, o, _) =>
        at == anno && o == args
      case _ =>
        false
    }
  }

  override def toString: String =
    s"@$anno(${args.foldLeft("")(_ + "," + _)})"
}

case class Assignment(assignTo: Access, op: String, assignFrom: Expression, lineCol: LineCol)
    extends Expression {
  override def hashCode(): Int = {
    var result = assignTo.hashCode()
    result = 31 * result + op.hashCode
    result = 31 * result + assignFrom.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Assignment(at, o, af, _) =>
        at == assignTo && af == assignFrom && op == o
      case _ =>
        false
    }
  }

  override def toString: String = s"Assignment($assignTo $op $assignFrom)"
}

/**
  * Literal
  *
  */
abstract class Literal(val literalType: Int, val literal: String, val lineCol: LineCol)
    extends Expression {
  override def hashCode(): Int = {
    var result = literalType
    val h = if (literal == null) 0 else literal.hashCode
    result = 31 * result + h
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    literal == obj.asInstanceOf[Literal].literal
  }

  override def toString: String = literal

}

object Literal {
  val NUMBER = 0
  val STRING = 1
  val BOOL = 2
}

/**
  * numberLiteral
  *
  */
case class NumberLiteral(override val literal: String, override val lineCol: LineCol)
    extends Literal(Literal.NUMBER, literal, lineCol)

/**
  * boolLiteral
  *
  */
case class BoolLiteral(override val literal: String, override val lineCol: LineCol)
    extends Literal(Literal.BOOL, literal, lineCol)

/**
  * stringLiteral
  *
  */
case class StringLiteral(override val literal: String, override val lineCol: LineCol)
    extends Literal(Literal.STRING, literal, lineCol)

/**
  * type
  *
  */
case class TypeOf(access: Access, lineCol: LineCol) extends Expression {
  override def hashCode(): Int = access.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    access == obj.asInstanceOf[TypeOf].access
  }

  override def toString: String = s"(type $access)"
}

/**
  * null
  *
  */
case class Null(lineCol: LineCol) extends Expression {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean = obj.isInstanceOf[Null]

  override def toString: String = "(null)"
}

/**
  * invocation
  *
  */
case class Invocation(access: Access, args: List[Expression], lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = if (access == null) 0 else access.hashCode()
    result = 31 * result + args.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    access == obj.asInstanceOf[Invocation].access && args == obj.asInstanceOf[Invocation].args
  }

  override def toString: String =
    s"Invocation($access(${args.foldLeft("")(_ + "." + _)}))"
}

/**
  * procedure
  *
  */
case class Procedure(statements: List[Statement], lineCol: LineCol) extends Expression {
  override def hashCode(): Int =
    if (statements != null)
      statements.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    statements == obj.asInstanceOf[Procedure].statements
  }

  override def toString: String = s"($statements)"
}

/**
  * undefined
  *
  */
case class UndefinedExp(lineCol: LineCol) extends Expression {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[UndefinedExp]

}

/**
  * as expression
  *
  */
case class AsType(exp: Expression, access: Access, lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = if (access == null) 0 else access.hashCode()
    result = 31 * result + exp.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[AsType].exp && access == obj.asInstanceOf[AsType].access
  }

  override def toString: String = s"return ($exp as $access)"
}

case class MethodStatement(
    name: String,
    modifiers: Set[Modifier],
    returnType: Access,
    params: ListBuffer[VariableDef],
    annos: Set[Anno],
    body: ListBuffer[Statement],
    lineCol: LineCol)
    extends Definition {

  override def hashCode(): Int = {
    var result = if (name == null) 0 else name.hashCode()
    result = 31 * result + (if (modifiers == null) 0 else modifiers.hashCode())
    result = 31 * result + (if (params == null) 0 else params.hashCode())
    result = 31 * result + (if (returnType == null) 0 else returnType.hashCode())
    result = 31 * result + (if (annos == null) 0 else annos.hashCode())
    result = 31 * result + (if (body == null) 0 else body.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case MethodStatement(n, m, r, p, a, b, _) =>
        (n == name
          && m == modifiers
          && p == params
          && r == returnType
          && a == annos
          && b == body)
      case _ =>
        false
    }
  }

  override def toString: String = {
    val sb = new StringBuilder("(")
    var isFirst = true
    for (a <- annos)
      sb.append(a).append(" ")
    for (a <- modifiers)
      sb.append(a).append(" ")
    sb.append(s"name $name(")
    for (p <- params) {
      if (isFirst) {
        isFirst = false
      } else {
        sb.append(",")
      }
      sb.append(p)
    }
    sb.append(")")
    if (returnType != null)
      sb.append(":").append(returnType)

    sb.append(" ").append(body).append(")")
    sb.toString()
  }
}

/**
  * index
  *
  */
case class Index(exp: Expression, args: List[Expression], lineCol: LineCol) extends Expression {
  override def hashCode(): Int = {
    var result = if (exp == null) 0 else exp.hashCode
    result = 31 * result + args.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[Index].exp && args == obj.asInstanceOf[Index].args
  }

  override def toString: String =
    s"($exp[${args.foldLeft("")(_ + "." + _).substring(1)}])"
}

/**
  * package
  *
  */
case class PackageRef(pkg: String, lineCol: LineCol) extends Expression {
  override def hashCode(): Int =
    if (pkg != null)
      pkg.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    pkg == obj.asInstanceOf[PackageRef].pkg
  }

  override def toString: String = s"($pkg)"
}

/**
  * packageDeclare
  *
  */
case class PackageDeclare(pkg: PackageRef, lineCol: LineCol) extends Pre {
  override def hashCode(): Int =
    if (pkg != null)
      pkg.hashCode()
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    pkg == obj.asInstanceOf[PackageDeclare].pkg
  }

  override def toString: String = s"(#$pkg)"
}

/**
  * MapExp
  *
  */
case class MapExp(map: Map[Expression, Expression], lineCol: LineCol) extends Expression {

  override def hashCode(): Int = map.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case MapExp(l, _) =>
        l == map
      case _ =>
        false
    }
  }

  override def toString: String =
    s"{${map.foldLeft("")(_ + "," + _).substring(1)}}"
}

/**
  * ArrayExp
  *
  */
case class ArrayExp(list: List[Expression], lineCol: LineCol) extends Expression {

  override def hashCode(): Int = list.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ArrayExp(l, _) =>
        l == list
      case _ =>
        false
    }
  }

  override def toString: String =
    s"[${list.foldLeft("")(_ + "," + _).substring(1)}]"
}

/**
  * Lambda
  *
  */
case class Lambda(params: List[VariableDef], statements: List[Statement], lineCol: LineCol)
    extends Expression {

  override def hashCode(): Int = {
    val result = if (params != null) params.hashCode else 0
    31 * result + (if (statements != null) statements.hashCode() else 0)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Lambda(p, s, _) =>
        p == params && statements == s
      case _ =>
        false
    }
  }

  override def toString: String =
    s"Lambda((${params.foldLeft("")(_ + "." + _).substring(1)})=>$statements)"
}

trait Pre extends Statement

/**
  * modifier
  *
  */
case class Modifier(modifier: String, lineCol: LineCol) extends Pre {

  override def hashCode(): Int =
    if (modifier != null)
      modifier.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    modifier == obj.asInstanceOf[Modifier].modifier
  }

  override def toString: String = s"($modifier)"
}

/**
  * return
  *
  */
case class Return(exp: Expression, lineCol: LineCol) extends Statement {
  override def hashCode(): Int =
    if (exp != null)
      exp.hashCode
    else
      0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    exp == obj.asInstanceOf[Return].exp
  }

  override def toString: String = s"($exp)"
}

/**
  * IfPair
  *
  */
case class IfPair(condition: Expression, body: List[Statement], lineCol: LineCol) {
  override def hashCode(): Int = {
    val result = if (condition == null) 0 else condition.hashCode()
    31 * result + (if (body == null) 0 else body.hashCode())
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case IfPair(c, b, _) =>
        c == condition && b == body
      case _ =>
        false
    }
  }
}

/**
  * If
  *
  */
case class IfStatement(ifs: List[IfPair], lineCol: LineCol) extends Statement {

  override def hashCode(): Int =
    if (ifs == null) 0 else ifs.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case IfStatement(i, _) =>
        i == ifs
      case _ =>
        false
    }
  }

  override def toString: String = {
    val sb = new StringBuilder("(")
    var isFirst = true
    for (p <- ifs) {
      if (isFirst) {
        isFirst = false
        sb.append("(if (").append(p.condition).append(p.body).append(")")
      } else {
        if (p.condition == null)
          sb.append("(else ").append(p.body).append(")")
        else
          sb.append("(elseif (").append(p.condition).append(")").append(p.body).append(")")
      }
    }
    sb.append(")")
    sb.toString()
  }

}

/**
  * For
  *
  */
case class ForStatement(name: String, exp: Expression, body: List[Statement], lineCol: LineCol)
    extends Statement {

  override def hashCode(): Int = {
    var result = if (name == null) 0 else name.hashCode()
    result = 31 * result + (if (exp == null) 0 else exp.hashCode())
    result = 31 * result + (if (body == null) 0 else body.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ForStatement(n, e, b, _) =>
        n == name && e == exp && b == body
      case _ =>
        false
    }
  }

  override def toString: String =
    s"(for $name @ $exp $body)"

}

/**
  * While
  *
  */
case class WhileStatement(
    condition: Expression,
    statements: List[Statement],
    doWhile: Boolean,
    lineCol: LineCol)
    extends Statement {
  override def hashCode(): Int = {
    var result = if (condition == null) 0 else condition.hashCode()
    result = 31 * result + (if (statements == null) 0 else statements.hashCode())
    result = 31 * result + (if (!doWhile) 0 else 1)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case WhileStatement(c, s, d, _) =>
        d == doWhile && c == condition && s == statements
      case _ =>
        false
    }
  }

  override def toString: String =
    if (doWhile)
      s"(do $statements while $condition)"
    else
      s"(while $condition $statements)"
}

/**
  * ImportDetails
  *
  */
case class ImportDetail(pkg: PackageRef, access: Access, importAll: Boolean) {
  override def hashCode(): Int = {
    var result = if (pkg == null) 0 else pkg.hashCode()
    result = 31 * result + (if (access == null) 0 else access.hashCode())
    result = 31 * result + (if (!importAll) 0 else 1)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ImportDetail(p, a, _) =>
        p == pkg && access == a
      case _ =>
        false
    }
  }

  override def toString: String =
    if (pkg == null) {
      if (importAll)
        access.toString + "._"
      else
        access.toString
    } else {
      if (importAll)
        pkg.toString + "._"
      else
        "invalid import"
    }

}

/**
  * Import
  *
  */
case class Import(importDetails: List[ImportDetail], lineCol: LineCol) extends Pre {
  override def hashCode(): Int =
    importDetails.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Import(p, _) =>
        p == importDetails
      case _ =>
        false
    }
  }

  override def toString: String =
    s"(#> $importDetails)"
}

/**
  * pass
  *
  */
case class Pass(lineCol: LineCol) extends Statement {
  override def hashCode(): Int = 0

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Pass]

  override def toString: String =
    s"(...)"
}

/**
  * StaticScope
  *
  */
case class StaticScope(statements: List[Statement], lineCol: LineCol) extends Statement {
  override def hashCode(): Int =
    if (statements == null) 0 else statements.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case StaticScope(i, _) =>
        i == statements
      case _ =>
        false
    }
  }

  override def toString: String =
    s"Static($statements)"
}

/**
  * ClassStatement
  *
  */
case class ClassStatement(
    name: String,
    modifiers: Set[Modifier],
    params: List[VariableDef],
    superWithInvocation: Invocation,
    superWithOutInvocation: List[Access],
    annos: Set[Anno],
    statements: List[Statement],
    lineCol: LineCol)
    extends Statement {

  override def hashCode(): Int = {
    var result = if (name == null) 0 else name.hashCode()
    result = 31 * result + (if (modifiers == null) 0 else modifiers.hashCode())
    result = 31 * result + (if (params == null) 0 else params.hashCode())
    result = 31 * result + (if (superWithInvocation == null) 0 else superWithInvocation.hashCode())
    result = 31 * result + (if (superWithOutInvocation == null) 0
                            else superWithOutInvocation.hashCode())
    result = 31 * result + (if (annos == null) 0 else annos.hashCode())
    result = 31 * result + (if (statements == null) 0 else statements.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ClassStatement(n, m, p, s, so, a, stmt, _) =>
        (n == name
          && m == modifiers
          && p == params
          && s == superWithInvocation
          && so == superWithOutInvocation
          && a == annos
          && stmt == statements)
      case _ =>
        false
    }
  }

  override def toString: String = {
    val sb = new StringBuilder("(")
    var isFirst = true
    for (a <- annos)
      sb.append(a).append(" ")
    for (a <- modifiers)
      sb.append(a).append(" ")
    sb.append(s"class $name(")
    for (p <- params) {
      if (isFirst) {
        isFirst = false
      } else {
        sb.append(",")
      }
      sb.append(p)
    }
    sb.append(")")
    if (superWithInvocation != null || superWithOutInvocation.nonEmpty)
      sb.append(" : ")
    isFirst = true
    if (superWithInvocation != null) {
      sb.append(superWithInvocation)
      isFirst = false
    }
    for (a <- superWithOutInvocation) {
      if (isFirst) {
        isFirst = false
      } else {
        sb.append(",")
      }
      sb.append(a)
    }
    sb.append(" ").append(statements).append(")")
    sb.toString()
  }
}

/**
  * Try
  *
  */
case class Try(
    statements: List[Statement],
    catches: List[Catch],
    varName: String,
    fin: List[Statement],
    lineCol: LineCol)
    extends Statement {
  override def hashCode(): Int = {
    var result = if (statements == null) 0 else statements.hashCode()
    result = 31 * result + (if (catches == null) 0 else catches.hashCode())
    result = 31 * result + (if (varName == null) 0 else varName.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Try(s, c, v, _, _) =>
        s == statements && c == catches && v == varName
      case _ =>
        false
    }
  }

  override def toString: String =
    s"(try $statements catch $varName ($catches) finally $fin)"
}

/**
  * Catch
  *
  */
case class Catch(exceptionTypes: List[Access], statements: List[Statement], lineCol: LineCol)
    extends Statement {
  override def hashCode(): Int = {
    val result = if (exceptionTypes == null) 0 else exceptionTypes.hashCode()
    31 * result + (if (statements == null) 0 else statements.hashCode())
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case Catch(e, s, _) =>
        e == exceptionTypes && s == statements
      case _ =>
        false
    }
  }

  override def toString: String =
    s"$exceptionTypes $statements"
}

/**
  * throwable
  *
  */
case class Throw(expression: Expression, lineCol: LineCol) extends Statement {
  override def hashCode(): Int =
    expression.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass) return false
    obj match {
      case o: Throw =>
        expression == o.expression
    }
  }

  override def toString: String =
    s"(throw $expression)"
}

/**
  * Definition
  *
  */
trait Definition extends Statement

case class VariableDef(
    name: String,
    modifiers: Set[Modifier],
    var vType: Access,
    var init: Expression,
    annos: Set[Anno],
    lineCol: LineCol)
    extends Definition
    with Expression {
  override def hashCode(): Int = {
    var result = name.hashCode
    result = 31 * result + (if (vType != null) vType.hashCode() else 0)
    result = 31 * result + (if (init != null) init.hashCode() else 0)
    result = 31 * result + modifiers.hashCode()
    result = 31 * result + annos.hashCode()
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass) return false
    obj match {
      case o: VariableDef =>
        name == o.name && vType == o.vType && init == o.init && modifiers == o.modifiers && annos == o.annos
    }
  }

  override def toString: String =
    s"VariableDef(${annos.foldLeft("")(_ + " " + _)}${modifiers.foldLeft("")(_ + " " + _)})($name)" +
      s"${if (vType != null) s":$vType"}${if (init != null) s" = $init"}"
}

/**
  * interface
  *
  */
case class InterfaceStatement(
    name: String,
    modifiers: Set[Modifier],
    superInterfaces: List[Access],
    annos: Set[Anno],
    statements: List[Statement],
    lineCol: LineCol)
    extends Statement {

  override def hashCode(): Int = {
    var result = if (name == null) 0 else name.hashCode()
    result = 31 * result + (if (modifiers == null) 0 else modifiers.hashCode())
    result = 31 * result + (if (superInterfaces == null) 0 else superInterfaces.hashCode())
    result = 31 * result + (if (annos == null) 0 else annos.hashCode())
    result = 31 * result + (if (statements == null) 0 else statements.hashCode())
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case InterfaceStatement(n, m, so, a, stmt, _) =>
        (n == name
          && m == modifiers
          && so == superInterfaces
          && a == annos
          && stmt == statements)
      case _ =>
        false
    }
  }

  override def toString: String = {
    val sb = new StringBuilder("(")
    var isFirst = true
    for (a <- annos)
      sb.append(a).append(" ")
    for (a <- modifiers)
      sb.append(a).append(" ")
    sb.append(s"interface $name(")
    sb.append(")")
    for (a <- superInterfaces) {
      if (isFirst) {
        isFirst = false
      } else {
        sb.append(",")
      }
      sb.append(a)
    }
    sb.append(" ").append(statements).append(")")
    sb.toString()
  }
}

/**
  * operation
  *
  */
trait Operation extends Expression {
  def operator(): String

  def expressions(): List[Expression]

  def invokeOn(): Int

  def isUnary(): Boolean
}

/**
  * UnaryOneVariableOperation
  *
  */
case class UnaryOneVariableOperation(operator: String, exp: Expression, lineCol: LineCol)
    extends Operation {
  override def expressions(): List[Expression] = List(exp)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = true

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (exp != null) exp.hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    operator == obj.asInstanceOf[UnaryOneVariableOperation].operator && exp == obj
      .asInstanceOf[UnaryOneVariableOperation]
      .exp
  }

  override def toString: String =
    s"($operator $exp)"
}

/**
  * OneVariableOperation
  *
  */
case class OneVariableOperation(operator: String, exp: Expression, lineCol: LineCol)
    extends Operation {
  override def expressions(): List[Expression] = List(exp)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = false

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (exp != null) exp.hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    operator == obj.asInstanceOf[OneVariableOperation].operator && exp == obj
      .asInstanceOf[OneVariableOperation]
      .exp
  }

  override def toString: String =
    s"($operator $exp)"
}

/**
  * TwoVariableOperation
  *
  */
case class TwoVariableOperation(
    operator: String,
    exp1: Expression,
    exp2: Expression,
    lineCol: LineCol)
    extends Operation {
  override def expressions(): List[Expression] = List(exp1, exp2)

  override def invokeOn(): Int = 0

  override def isUnary(): Boolean = false

  override def hashCode(): Int = {
    var result = if (operator != null) operator.hashCode() else 0
    result = 31 * result + (if (expressions() != null) expressions().hashCode() else 0)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case TwoVariableOperation(o, e1, e2, _) =>
        o == operator && e1 == exp1 && e2 == exp2
      case _ =>
        false
    }
  }

  override def toString: String =
    s"($exp1 $operator $exp2)"
}
