package latte

import java.lang.annotation.{ElementType, Target}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait SAnnotationPresentable {
  def annos(): ListBuffer[SAnno]
}

trait Value {
  def typeOf(): STypeDef
}

trait LeftValue extends Value {
  def canChange: Boolean
}

trait Instruction {
  def lineCol(): LineCol
}

trait PrimitiveValue extends Value

trait ConstantValue extends Value {
  def getByte: Array[Byte]
}

class STypeDef(val lineCol: LineCol, val annos: ListBuffer[SAnno]) extends SAnnotationPresentable {
  var pkg: String = _
  var fullName: String = ""

  def this(lineCol: LineCol) = {
    this(lineCol, ListBuffer())
  }

  def isAssignableFrom(cls: STypeDef): Boolean =
    cls match {
      case null => throw new NullPointerException()
      case _: NullTypeDef =>
        !this.isInstanceOf[PrimitiveTypeDef]
      case _ => cls == this
    }
}

case class SAnno() extends Value {
  var annoDef: SAnnoDef = _
  var present: SAnnotationPresentable = _
  val valueMap: mutable.HashMap[SAnnoField, Value] = mutable.HashMap()
  val alreadyCompiledAnnotationValueMap: mutable.HashMap[String, AnyRef] = mutable.HashMap()

  override def typeOf(): SAnnoDef = annoDef

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(typeOf().fullName).append("(")
    var isFirst = true
    for (f <- valueMap) {
      if (isFirst)
        isFirst = false
      else
        sb.append(",")
      sb.append(f._1.name).append("=").append(f._2)
    }
    sb.append(")")
    sb.toString()
  }
}

case class SArrayTypeDef() extends STypeDef(LineCol.SYNTHETIC) {
  var sType: STypeDef = _
  var dimension: Int = _
  fullName = if (fullName == null) rebuildFullName() else fullName

  private def rebuildFullName(): String = {
    if (null == sType) return ""
    val sb = new mutable.StringBuilder()

    Range(0, dimension).foreach(sb.append("["))
    sType match {
      case t: PrimitiveTypeDef =>
        t match {
          case _: ByteTypeDef => sb.append("B")
          case _: CharTypeDef => sb.append("C")
          case _: DoubleTypeDef => sb.append("D")
          case _: FloatTypeDef => sb.append("F")
          case _: IntTypeDef => sb.append("I")
          case _: LongTypeDef => sb.append("L")
          case _: ShortTypeDef => sb.append("S")
          case _: BoolTypeDef => sb.append("B")
          case _ =>
        }
      case _ =>
        sb.append("L").append(sType.fullName).append(";")
    }
    sb.toString()
  }
}

case class SAnnoDef() extends STypeDef(LineCol.SYNTHETIC) {
  val annoFields: ListBuffer[SAnnoField] = ListBuffer()
  val modifiers: ListBuffer[SModifier] = ListBuffer()

  def canPresentOn(sType: ElementType): Boolean = {
    val name = fullName
    try {
      val cls = Class.forName(name)
      val annotations = cls.getAnnotations
      annotations.foreach {
        case target: Target =>
          val eTypes = target.value
          eTypes.foreach { t =>
            if (t == sType) return true
          }
          return false
        case _ =>
      }
      true
    } catch {
      case e: ClassNotFoundException =>
        throw new LtBug(e)
    }
  }
}

class SAnnoField() extends SMethodDef(LineCol.SYNTHETIC) {
  var sType: STypeDef = _
  var defaultValue: Value = _

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(sType.fullName).append(" ").append(name).append("()")
    if (defaultValue != null)
      sb.append(" default ").append(defaultValue).append(";")
    sb.toString()
  }

}

abstract class PrimitiveTypeDef() extends STypeDef(LineCol.SYNTHETIC)

/**
  * int
  */
case class IntTypeDef() extends PrimitiveTypeDef() {
  fullName = "int"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[CharTypeDef] || cls.isInstanceOf[ByteTypeDef] || cls.isInstanceOf[ShortTypeDef]
  }
}

object IntTypeDef {
  def t: IntTypeDef = IntTypeDef()

  def get(): IntTypeDef = t
}

/**
  * float
  */
case class FloatTypeDef() extends PrimitiveTypeDef() {
  fullName = "float"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[IntTypeDef] || IntTypeDef.get().isAssignableFrom(cls)
  }
}

object FloatTypeDef {
  def t: FloatTypeDef = FloatTypeDef()

  def get(): FloatTypeDef = t
}

/**
  * double
  */
case class DoubleTypeDef() extends PrimitiveTypeDef() {
  fullName = "double"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[FloatTypeDef] || cls
      .isInstanceOf[LongTypeDef] || FloatTypeDef.get().isAssignableFrom(cls)
  }
}

object DoubleTypeDef {
  def t: DoubleTypeDef = DoubleTypeDef()

  def get(): DoubleTypeDef = t
}

/**
  * long
  */
case class LongTypeDef() extends PrimitiveTypeDef() {
  fullName = "long"

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    cls.isInstanceOf[IntTypeDef] || IntTypeDef.get().isAssignableFrom(cls)
  }
}

object LongTypeDef {
  def t: LongTypeDef = LongTypeDef()

  def get(): LongTypeDef = t
}

/**
  * char
  */
case class CharTypeDef() extends PrimitiveTypeDef() {
  fullName = "char"
}

object CharTypeDef {
  def t: CharTypeDef = CharTypeDef()

  def get(): CharTypeDef = t
}

/**
  * short
  */
case class ShortTypeDef() extends PrimitiveTypeDef() {
  fullName = "short"
}

object ShortTypeDef {
  def t: ShortTypeDef = ShortTypeDef()

  def get(): ShortTypeDef = t
}

/**
  * byte
  */
case class ByteTypeDef() extends PrimitiveTypeDef() {
  fullName = "byte"
}

object ByteTypeDef {
  def t: ByteTypeDef = ByteTypeDef()

  def get(): ByteTypeDef = t
}

/**
  * boolean
  */
case class BoolTypeDef() extends PrimitiveTypeDef() {
  fullName = "boolean"
}

object BoolTypeDef {
  def t: BoolTypeDef = BoolTypeDef()

  def get(): BoolTypeDef = t
}

/**
  * null
  */
case class NullTypeDef() extends STypeDef(LineCol.SYNTHETIC) {
  fullName = "null"
}

object NullTypeDef {
  def t: NullTypeDef = NullTypeDef()

  def get(): NullTypeDef = t
}

case class VoidType() extends STypeDef(LineCol.SYNTHETIC) {
  fullName = "void"
}

object VoidType {
  def t: VoidType = VoidType()

  def get(): VoidType = t
}

abstract class SMember(lineCol: LineCol) extends SAnnotationPresentable {
  val modifiers: ListBuffer[SModifier] = ListBuffer()
  var declaringType: STypeDef = _
  var annos: ListBuffer[SAnno] = ListBuffer()
}

abstract class SInvokable(lineCol: LineCol) extends SMember(lineCol) {
  val parameters: ListBuffer[SParameter] = ListBuffer()
  var returnType: STypeDef = null
  val statements: ListBuffer[Instruction] = ListBuffer()
  val exceptionTables: ListBuffer[ExceptionTable] = ListBuffer()
}

case class SParameter() extends LeftValue with SAnnotationPresentable {
  val annos: ListBuffer[SAnno] = ListBuffer()
  var name: String = _
  var sType: STypeDef = _
  val canChange: Boolean = true
  var target: SInvokable = _

  override def typeOf(): STypeDef = sType

  override def toString: String =
    s"${if (!canChange) "final" else ""}${sType.fullName} $name"
}

case class SMethodDef(lineCol: LineCol) extends SInvokable(lineCol) {
  var name: String = _
  val overRide: ListBuffer[SMethodDef] = ListBuffer()
  val overRidden: ListBuffer[SMethodDef] = ListBuffer()

  override def toString: String = {
    val modifiers = this.modifiers.map(_.toString.toLowerCase()).foldLeft("")(_ + " " + _)
    val temp = returnType.fullName + " " + declaringType.fullName + "." + name + "("
    val params = parameters.foldLeft("")(_ + "," + _)
    modifiers + temp + params + ")"
  }

}

case class SClassDef(override val lineCol: LineCol) extends STypeDef(lineCol) {
  val modifiers: ListBuffer[SModifier] = ListBuffer()
  val fields: ListBuffer[SFieldDef] = ListBuffer()
  val constructors: ListBuffer[SConstructorDef] = ListBuffer()
  val methods: ListBuffer[SMethodDef] = ListBuffer()
  val superInterfaces: ListBuffer[SInterfaceDef] = ListBuffer()
  val staticStatements: ListBuffer[Instruction] = ListBuffer()
  val staticExceptionTable: ListBuffer[ExceptionTable] = ListBuffer()
  var parent: SClassDef = _
}

case class SFieldDef(lineCol: LineCol) extends SMember(lineCol) with LeftValue {
  var sType: STypeDef = _
  var name: String = _

  override def canChange: Boolean = !modifiers.contains(SModifier.FINAL)

  override def typeOf(): STypeDef = sType
}

case class SConstructorDef(lineCol: LineCol) extends SInvokable(lineCol) {
  returnType = VoidType.get()
}

case class SInterfaceDef(override val lineCol: LineCol) extends STypeDef(lineCol) {
  val fields: ListBuffer[SFieldDef] = ListBuffer()
  val methods: ListBuffer[SMethodDef] = ListBuffer()
  val modifiers: ListBuffer[SModifier] = ListBuffer()
  val superInterfaces: ListBuffer[SInterfaceDef] = ListBuffer()
  val staticStatements: ListBuffer[Instruction] = ListBuffer()
  val staticExceptionTable: ListBuffer[ExceptionTable] = ListBuffer()

  override def isAssignableFrom(cls: STypeDef): Boolean = {
    if (super.isAssignableFrom(cls)) return true
    val list: mutable.Stack[SInterfaceDef] = mutable.Stack()
    cls match {
      case s: SClassDef =>
        list.pushAll(s.superInterfaces)
      case s: SInterfaceDef =>
        list.pushAll(s.superInterfaces)
      case _ =>
    }
    while (list.nonEmpty) {
      val i = list.pop()
      if (isAssignableFrom(i)) return true
      list.pushAll(i.superInterfaces)
    }
    false
  }

  override def toString: String = {
    val modifier = modifiers.foldLeft("")(_ + " " + _)
    val temp = s"interface$fullName${if (superInterfaces.nonEmpty) "extends" else ""}"
    val interfaces = superInterfaces.foldLeft("")(_ + "," + _)
    modifier + temp + interfaces
  }
}

case class IntValue(value: Int) extends PrimitiveValue with ConstantValue {
  override def getByte: Array[Byte] =
    Array(
      (value & 0xff).toByte,
      ((value & 0xff00) >> 8).toByte,
      ((value & 0xff0000) >> 16).toByte,
      ((value & 0xff000000) >> 24).toByte,
    )

  override def typeOf(): STypeDef = IntTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = value

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case IntValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class LocalVariable(sType: STypeDef, canChange: Boolean) extends LeftValue {
  override def typeOf(): STypeDef = sType
}

case class ExceptionTable(from: Instruction, to: Instruction, target: Instruction, sType: STypeDef)

case class SModifier(value: Int)

object SModifier {
  val PUBLIC = SModifier(0x0001)
  val PRIVATE = SModifier(0x0002)
  val PROTECTED = SModifier(0x0004)
  val STATIC = SModifier(0x0008)
  val FINAL = SModifier(0x0010)
  val VOLATILE = SModifier(0x0040)
  val TRANSIENT = SModifier(0x0080)
  val ABSTRACT = SModifier(0x0400)
  val SYNTHETIC = SModifier(0x1000)
  val ENUM = SModifier(0x4000)
  val NATIVE = SModifier(0x0100)
  val STRUCT = SModifier(0x0800)
  val SYNCHRONIZED = SModifier(0x0200)
}
