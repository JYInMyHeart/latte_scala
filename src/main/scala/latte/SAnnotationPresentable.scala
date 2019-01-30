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
  var returnType: STypeDef = _
  val statements: ListBuffer[Instruction] = ListBuffer()
  val exceptionTables: ListBuffer[ExceptionTable] = ListBuffer()
}

case class SParameter() extends LeftValue with SAnnotationPresentable {
  val annos: ListBuffer[SAnno] = ListBuffer()
  var name: String = _
  var sType: STypeDef = _
  var canChange: Boolean = true
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

case class LongValue(value: Long) extends PrimitiveValue with ConstantValue {
  override def getByte: Array[Byte] =
    Array(
      (value & 0xff).toByte,
      ((value >> 8) & 0xff).toByte,
      ((value >> 16) & 0xff).toByte,
      ((value >> 24) & 0xff).toByte,
      ((value >> 32) & 0xff).toByte,
      ((value >> 40) & 0xff).toByte,
      ((value >> 48) & 0xff).toByte,
      ((value >> 56) & 0xff).toByte
    )

  override def typeOf(): STypeDef = LongTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Long = value

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case LongValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class ShortValue(value: Short) extends PrimitiveValue {

  override def typeOf(): STypeDef = ShortTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Short = value

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ShortValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class FloatValue(value: Float) extends PrimitiveValue with ConstantValue {
  override def getByte: Array[Byte] = {
    val data = java.lang.Float.floatToIntBits(value)
    Array(
      (data & 0xff).toByte,
      ((data & 0xff00) >> 8).toByte,
      ((data & 0xff0000) >> 16).toByte,
      ((data & 0xff000000) >> 24).toByte,
    )
  }

  override def typeOf(): STypeDef = FloatTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = if (value != +0.0f) java.lang.Float.floatToIntBits(value) else 0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case FloatValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class DoubleValue(value: Double) extends PrimitiveValue with ConstantValue {
  override def getByte: Array[Byte] = {
    val data = java.lang.Double.doubleToLongBits(value)
    Array(
      (data & 0xff).toByte,
      ((data >> 8) & 0xff).toByte,
      ((data >> 16) & 0xff).toByte,
      ((data >> 24) & 0xff).toByte,
      ((data >> 32) & 0xff).toByte,
      ((data >> 40) & 0xff).toByte,
      ((data >> 48) & 0xff).toByte,
      ((data >> 56) & 0xff).toByte
    )
  }

  override def typeOf(): STypeDef = DoubleTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = {
    val temp = java.lang.Double.doubleToLongBits(value)
    (temp ^ (temp >>> 32)).toInt
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case DoubleValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class CharValue(value: Char) extends PrimitiveValue {

  override def typeOf(): STypeDef = CharTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = value.toInt

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case CharValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class ByteValue(value: Byte) extends PrimitiveValue {

  override def typeOf(): STypeDef = ByteTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = value.toInt

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case ByteValue(i) =>
        i == value
      case _ =>
        false
    }
  }
}

case class BoolValue(value: Boolean) extends PrimitiveValue {

  override def typeOf(): STypeDef = BoolTypeDef.get()

  override def toString: String = value.toString

  override def hashCode(): Int = if (value) 1 else 0

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case BoolValue(i) =>
        i == value
      case _ =>
        false
    }
  }

  def getValue(): Int = if (value) 1 else 0
}

case class StringConstantValue(value: String) extends ConstantValue with Value {
  var sType: STypeDef = _

  override def getByte: Array[Byte] = value.getBytes

  override def typeOf(): STypeDef = sType

  override def equals(obj: Any): Boolean = {
    if (obj == null || getClass != obj.getClass)
      return false
    obj match {
      case StringConstantValue(i) =>
        i == value
      case _ =>
        false
    }
  }

  override def hashCode(): Int = value.hashCode
}


case class NullValue() extends Value{
  override def typeOf(): STypeDef = NullTypeDef.get()

  override def toString: String = "null"
}

object NullValue{
  val nullValue =  NullValue()
  def get(): NullValue = nullValue
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
  val STRICT = SModifier(0x0800)
  val SYNCHRONIZED = SModifier(0x0200)
}
