package latte

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Ins {

  case class ANewArray(arrayType: SArrayTypeDef, componentType: STypeDef, count: IntValue)
      extends Value {
    val initValues: ListBuffer[Value] = ListBuffer()

    override def typeOf(): STypeDef = arrayType
  }

  case class ArrayLength(arrayValue: Value, lineCol: LineCol) extends Value with Instruction {
    override def typeOf(): STypeDef = IntTypeDef.get()
  }

  case class AThrow(exception: Value, lineCol: LineCol) extends Instruction

  case class Cast(sType: STypeDef, value: Value, caseMode: Int, lineCol: LineCol)
      extends Value
      with Instruction {
    override def typeOf(): STypeDef = sType
  }

  object Cast {
    val CAST_INT_TO_LONG = 0x85
    val CAST_INT_TO_FLOAT = 0x86
    val CAST_INT_TO_DOUBLE = 0x87

    val CAST_LONG_TO_INT = 0x88
    val CAST_LONG_TO_FLOAT = 0x89
    val CAST_LONG_TO_DOUBLE = 0x8A

    val CAST_FLOAT_TO_INT = 0x8B
    val CAST_FLOAT_TO_LONG = 0x8C
    val CAST_FLOAT_TO_DOUBLE = 0x8D

    val CAST_DOUBLE_TO_INT = 0x8E
    val CAST_DOUBLE_TO_LONG = 0x8F
    val CAST_DOUBLE_TO_FLOAT = 0x90

    val CAST_INT_TO_BYTE = 0x91
    val CAST_INT_TO_CHAR = 0x92
    val CAST_INT_TO_SHORT = 0x93
  }

  case class CheckCast(theValueToCheck: Value, requiredType: STypeDef, lineCol: LineCol)
      extends Instruction
      with Value {
    override def typeOf(): STypeDef = requiredType
  }

  case class ExStore(ex: LeftValue, scope: SemanticScope) extends Instruction {
    val index = scope.getIndex(ex)

    override def lineCol(): LineCol = null
  }

  case class GetClass(targetType: STypeDef, classClassDef: STypeDef) extends Value {
    override def typeOf(): STypeDef = classClassDef
  }

  case class GetField(field: SFieldDef, lineCol: LineCol, value: Value)
      extends Value
      with Instruction {
    override def typeOf(): STypeDef = field.typeOf()
  }

  case class GetStatic(field: SFieldDef, lineCol: LineCol) extends Value with Instruction {
    override def typeOf(): STypeDef = field.typeOf()
  }

  case class Goto(gotoIns: Instruction) extends Instruction {
    override def lineCol(): LineCol = null
  }

  case class IfEq(condition: Value, gotoIns: Instruction, lineCol: LineCol) extends Instruction {}

  case class IfNe(condition: Value, gotoIns: Instruction, lineCol: LineCol) extends Instruction {}

  class Invoke(val invokable: SInvokable, val lineCol: LineCol) extends Instruction with Value {
    val arguments: ListBuffer[Value] = ListBuffer[Value]()

    override def typeOf(): STypeDef = invokable.returnType
  }

  case class InvokeDynamic(
      bootstrapMethod: SInvokable,
      methodName: String,
      args: ListBuffer[Value],
      returnType: STypeDef,
      indyType: Int,
      override val lineCol: LineCol)
      extends Invoke(bootstrapMethod, lineCol) {}

  class InvokeWithTarget(target: Value, invokable: SInvokable, lineCol: LineCol)
      extends Invoke(invokable, lineCol)
      with Instruction
      with Value {}

  case class InvokeSpecial(
      target: Value,
      override val invokable: SInvokable,
      override val lineCol: LineCol)
      extends InvokeWithTarget(target, invokable, lineCol) {}

  case class InvokeInterface(
      target: Value,
      override val invokable: SInvokable,
      override val lineCol: LineCol)
      extends InvokeWithTarget(target, invokable, lineCol) {}

  case class InvokeStatic(override val invokable: SInvokable, override val lineCol: LineCol)
      extends Invoke(invokable, lineCol) {}

  case class InvokeVirtual(
      target: Value,
      override val invokable: SInvokable,
      override val lineCol: LineCol)
      extends InvokeWithTarget(target, invokable, lineCol) {}

  case class LogicAnd(b1: Value, b2: Value, lineCol: LineCol) extends Value with Instruction {
    override def typeOf(): STypeDef = BoolTypeDef.get()
  }

  case class LogicOr(b1: Value, b2: Value, lineCol: LineCol) extends Value with Instruction {
    override def typeOf(): STypeDef = BoolTypeDef.get()
  }

  case class MonitorEnter(valueToMonitor: Value, lineCol: LineCol) extends Instruction {
    var index: Int = _

    def this(valueToMonitor: Value, scope: SemanticScope, lineCol: LineCol) = {
      this(valueToMonitor, lineCol)
      val localVariable = LocalVariable(valueToMonitor.typeOf(), canChange = false)
      scope.putLeftValue(scope.generateTempName, localVariable)
      index = scope.getIndex(localVariable).getOrElse(0)
    }
  }

  case class MonitorExit(enterInstruction: MonitorEnter, lineCol: LineCol) extends Instruction {
    def this(enterInstruction: MonitorEnter) = {
      this(enterInstruction, enterInstruction.lineCol)
    }
  }

  case class New(constructor: SConstructorDef, lineCol: LineCol) extends Value with Instruction {

    val args: ListBuffer[Value] = ListBuffer()

    override def typeOf(): STypeDef = constructor.declaringType
  }

  case class NewArray(count: IntValue, mode: Int, storeMode: Int, sType: STypeDef) extends Value {

    val initValues: ListBuffer[Value] = ListBuffer()

    override def typeOf(): STypeDef = sType
  }

  case class NewList(linkedListType: STypeDef) extends Value {
    val initValues: ListBuffer[Value] = ListBuffer()

    override def typeOf(): STypeDef = linkedListType
  }

  case class NewMap(linkedListClass: STypeDef) extends Value {
    val initValues: mutable.LinkedHashMap[Value, Value] = mutable.LinkedHashMap()

    override def typeOf(): STypeDef = linkedListClass
  }

  case class Nop(lineCol: LineCol) extends Instruction() {
    def this() = {
      this(null)
    }
  }

  case class OneVarOp(value: Value, op: Int, sType: STypeDef, lineCol: LineCol)
      extends Value
      with Instruction {
    override def typeOf(): STypeDef = sType
  }

  object OneVarOp {
    val Ineg = 0x74
    val Lneg = 0x75
    val Fneg = 0x76
    val Dneg = 0x77
  }

  case class Pop(lineCol: LineCol) extends Instruction() {
    def this() = {
      this(null)
    }
  }

  case class PutField(field: SFieldDef, obj: Value, value: Value, lineCol: LineCol)
      extends Instruction {}

  case class PutStatic(field: SFieldDef, value: Value, lineCol: LineCol) extends Instruction {}

  case class TALoad(arr: Value, index: Value, modeValue: Int, lineCol: LineCol)
      extends Value
      with Instruction {

    var mode: Int = modeValue

    def this(arr: Value, index: Value, lineCol: LineCol) = {
      this(arr, index, 0, lineCol)
      val sType = arr.typeOf().asInstanceOf[SArrayTypeDef].sType
      import TALoad._
      sType match {
        case t: BoolTypeDef => mode = Baload
        case t: ByteTypeDef => mode = Baload
        case t: ShortTypeDef => mode = Saload
        case t: IntTypeDef => mode = Iaload
        case t: LongTypeDef => mode = Laload
        case t: FloatTypeDef => mode = Faload
        case t: DoubleTypeDef => mode = Daload
        case t: CharTypeDef => mode = Caload
        case _ => mode = Aaload
      }
    }

    override def typeOf(): STypeDef = arr.typeOf().asInstanceOf[SArrayTypeDef].sType
  }

  object TALoad {
    var Baload = 0
    var Saload = 0
    var Iaload = 0
    var Laload = 0
    var Faload = 0
    var Daload = 0
    var Caload = 0
    var Aaload = 0
  }

  case class TLoad(value: LeftValue, var mode: Int, var index: Int, lineCol: LineCol)
      extends Value
      with Instruction {
    def this(value: LeftValue, scope: SemanticScope, lineCol: LineCol) = {
      this(value, 0, 0, lineCol)
      import TLoad._
      mode = value.typeOf() match {
        case x: IntTypeDef => Iload
        case x if IntTypeDef.get().isAssignableFrom(x) => Iload
        case x: FloatTypeDef => Fload
        case x: LongTypeDef => Lload
        case x: DoubleTypeDef => Dload
        case x: BoolTypeDef => Iload
        case _ => Aload
      }
      index = scope.getIndex(value).getOrElse(0)
    }

    override def typeOf(): STypeDef = value.typeOf()
  }

  object TLoad {
    var Iload = 0x15
    var Lload = 0x16
    var Fload = 0x17
    var Dload = 0x18
    var Aload = 0x19
  }

  case class This(sTypeDef: STypeDef) extends Value {
    override def typeOf(): STypeDef = sTypeDef
  }

  case class TReturn(value: Value, returnIns: Int, lineCol: LineCol) extends Instruction

  object TReturn {
    val Return = 0xb1
    val AReturn = 0xb0
    val DReturn = 0xaf
    val FReturn = 0xae
    val IReturn = 0xac
    val LReturn = 0xad
  }

  case class TStore(leftValue: LeftValue, newValue: Value, lineCol: LineCol) extends Instruction {

    import TStore._

    var mode: Int = _
    var index: Int = _

    def this(leftValue: LeftValue, newValue: Value, scope: SemanticScope, lineCol: LineCol) = {
      this(leftValue, newValue, lineCol)

      newValue.typeOf() match {
        case _: IntTypeDef => mode = Istore
        case i if IntTypeDef.get().isAssignableFrom(i) => mode = Istore
        case _: FloatTypeDef => mode = Fstore
        case _: LongTypeDef => mode = Lstore
        case _: DoubleTypeDef => mode = Dstore
        case _: BoolTypeDef => mode = Istore
        case _ => mode = Astore
      }
      index = scope.getIndex(leftValue).getOrElse(0)
    }
  }

  object TStore {
    val Astore = 0x3a
    val Dstore = 0x39
    val Fstore = 0x38
    val Lstore = 0x37
    val Istore = 0x36
  }

  case class ValuePack() extends Value with Instruction {
    val instructions: ListBuffer[Instruction] = ListBuffer()
    var sType: STypeDef = _
    override def typeOf(): STypeDef =
      if (sType == null) {
        if (instructions.isEmpty)
          return null
        val ins = instructions.last
        ins match {
          case value: Value => value.typeOf()
          case _ => null
        }
      } else
        sType

    override def lineCol(): LineCol =
      if (instructions.isEmpty) LineCol.SYNTHETIC
      else instructions.last.lineCol()

  }

}
