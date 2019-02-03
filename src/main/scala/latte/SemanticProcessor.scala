package latte

import java.lang.annotation.ElementType
import java.lang.reflect.{AnnotatedElement, Member}

import latte.Ins._
import latte.SModifier.{ABSTRACT, FINAL, PUBLIC}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

class SemanticProcessor(mapOfStatements: mutable.HashMap[String, List[Statement]]) {
  private val types: mutable.HashMap[String, STypeDef] = mutable.HashMap(
    "int" -> IntTypeDef.get(),
    "short" -> ShortTypeDef.get(),
    "byte" -> ByteTypeDef.get(),
    "boolean" -> BoolTypeDef.get(),
    "float" -> FloatTypeDef.get(),
    "long" -> LongTypeDef.get(),
    "char" -> CharTypeDef.get(),
    "double" -> DoubleTypeDef.get(),
  )
  private val originalClasses: mutable.HashMap[String, ClassStatement] = mutable.HashMap()
  private val originalInterfaces: mutable.HashMap[String, InterfaceStatement] = mutable.HashMap()
  private val methodToStatements: mutable.HashMap[SMethodDef, ListBuffer[Statement]] =
    mutable.HashMap()
  private val fileNameToImport: mutable.HashMap[String, ListBuffer[Import]] = mutable.HashMap()
  private val typeDefSet: mutable.HashSet[STypeDef] = mutable.HashSet()
  private val annotationRecorder: mutable.HashMap[SAnno, Anno] = mutable.HashMap()

  private lazy val langCastToThrowable: SMethodDef =
    getTypeWithName("lt.lang.Lang", LineCol.SYNTHETIC)
      .asInstanceOf[SClassDef]
      .methods
      .find(_.name == "castToThrowable")
      .orNull

  def parseAnnos(
      annos: Set[Anno],
      sClassDef: SAnnotationPresentable,
      imports: ListBuffer[Import],
      TYPE: ElementType): Unit =
    for (anno <- annos) {
      val annoType = getTypeWithAccess(anno.anno, imports)
      if (!annoType.asInstanceOf[SAnnoDef].canPresentOn(TYPE))
        throw new SyntaxException(s"annotation $annoType cannot present on $TYPE", anno.lineCol)
      val s = SAnno()
      s.annoDef = annoType.asInstanceOf[SAnnoDef]
      s.present = sClassDef
      sClassDef.annos() += s
      annotationRecorder += s -> anno
    }

  def parseParameters(
      params: List[VariableDef],
      i: Int,
      constructor: SInvokable,
      imports: ListBuffer[Import],
      allowAccessModifier: Boolean): Unit =
    for (j <- 0 until i) {
      val v = params(j)
      val param = SParameter()
      param.name = v.name
      param.target = constructor
      var sType: STypeDef = null
      if (v.vType == null) {
        sType = getTypeWithName("java.lang.Object", v.lineCol)
      } else {
        sType = getTypeWithAccess(v.vType, imports)
      }
      param.sType = sType
      v.modifiers.foreach { m =>
        m.modifier match {
          case "val" => param.canChange = false
          case x if x == "pub" || x == "pri" || x == "pro" || x == "pkg" =>
            if (!allowAccessModifier)
              throw new SyntaxException(
                "access modifiers for parameters are only allowed on class constructing parameters",
                m.lineCol)
          case _ =>
            throw UnexpectedTokenException(
              "valid modifier for parameters (val)",
              m.toString,
              m.lineCol)
        }
      }
      parseAnnos(v.annos, param, imports, ElementType.PARAMETER)
      constructor.parameters += param
    }

  def boxPrimitive(primitive: Value, lineCol: LineCol): InvokeStatic = {
    assert(primitive.typeOf().isInstanceOf[PrimitiveTypeDef])

    def generateInvokeStatic(sType: String, sTypeDef: PrimitiveTypeDef): InvokeStatic = {
      val aByte = getTypeWithName(s"java.lang.$sType", lineCol).asInstanceOf[SClassDef]
      val valueOf: SMethodDef = aByte.methods
        .find(x =>
          x.name == "valueOf" && x.parameters.size == 1 && x.parameters.head.sType == sTypeDef)
        .orNull
      if (valueOf == null)
        throw new LtBug(s"java.lang.$sType.valueOf(${sType.toLowerCase}) should exist")
      val invokeStatic = InvokeStatic(valueOf, lineCol)
      invokeStatic.arguments += primitive
      invokeStatic
    }

    primitive.typeOf() match {
      case _: ByteTypeDef => generateInvokeStatic("Byte", ByteTypeDef.get())
      case _: BoolTypeDef => generateInvokeStatic("Boolean", BoolTypeDef.get())
      case _: CharTypeDef => generateInvokeStatic("Char", CharTypeDef.get())
      case _: DoubleTypeDef => generateInvokeStatic("Double", DoubleTypeDef.get())
      case _: FloatTypeDef => generateInvokeStatic("Float", FloatTypeDef.get())
      case _: IntTypeDef => generateInvokeStatic("Int", IntTypeDef.get())
      case _: LongTypeDef => generateInvokeStatic("Long", LongTypeDef.get())
      case _: ShortTypeDef => generateInvokeStatic("Short", ShortTypeDef.get())
      case _ =>
        throw new LtBug("primitive can only be byte/boolean/char/double/float/int/short/long")
    }
  }

  def findMethodFromTypeWithArguements(
      lineCol: LineCol,
      name: String,
      argList: ListBuffer[Value],
      value: STypeDef,
      typeOf: STypeDef,
      FIND_MODE_ANY: Int,
      methodsToInvoke: ListBuffer[SMethodDef],
      checkSuper: Boolean) = ???

  def isGetFieldAtRuntime(target: Value): Boolean = ???

  def getInvokeDynamicBootstrapMethod(): SInvokable = ???

  def parseValueFromInvocation(exp: Invocation, scope: SemanticScope): Value = {
    assert(scope.typeOf.orNull.isInstanceOf[SClassDef])
    val listTemp = for (arg <- exp.args) yield parseValueFromExpression(arg, null, scope)
    val argList = ListBuffer(listTemp: _*)
    val methodsToInvoke: ListBuffer[SMethodDef] = ListBuffer()
    var innerMethod: MethodRecorder = null
    var target: Value = null

    val imports = fileNameToImport(exp.lineCol.fileName)
    val access = exp.access
    if (access.expression == null) {
      val r = scope.innerMethodMap(access.name)
      if (null != r) {
        val m = r.method
        if (r.paramCount == argList.size) {
          val inc = m.parameters.size - r.paramCount
          var canUse = true
          for (i <- 0 until r.paramCount) {
            val pType = m.parameters(i + inc).typeOf()
            val aType = argList(i).typeOf()
            if (!pType.isAssignableFrom(aType))
              canUse = false
          }
          if (canUse)
            innerMethod = r
        }
      }
      findMethodFromTypeWithArguements(
        access.lineCol,
        access.name,
        argList,
        if (scope.aThis == null) scope.typeOf.orNull else scope.aThis.typeOf(),
        scope.typeOf.orNull,
        SemanticProcessor.FIND_MODE_ANY,
        methodsToInvoke,
        checkSuper = true
      )

      if (methodsToInvoke.isEmpty) {
        for (im <- imports) {
          val innerLoop = new Breaks
          innerLoop.breakable {
            for (detail <- im.importDetails) {
              if (methodsToInvoke.nonEmpty) innerLoop.break()
              if (detail.importAll && detail.pkg == null) {
                val sType = getTypeWithAccess(detail.access, imports)
                findMethodFromTypeWithArguements(
                  access.lineCol,
                  access.name,
                  argList,
                  null,
                  sType,
                  SemanticProcessor.FIND_MODE_STATIC,
                  methodsToInvoke,
                  checkSuper = true
                )
              }
            }
          }
        }
      }
    }
    var doInvokeSpecial = false
    if (methodsToInvoke.isEmpty) {
      if (access.expression != null) {
        access.expression match {
          case access1: Access if access1.name == "this" =>
            if (access1.expression == null) {
              findMethodFromTypeWithArguements(
                access.lineCol,
                access.name,
                argList,
                if (scope.aThis == null) scope.typeOf.orNull else scope.aThis.typeOf(),
                scope.typeOf.orNull,
                SemanticProcessor.FIND_MODE_STATIC,
                methodsToInvoke,
                checkSuper = true
              )
            } else if (access1.expression.isInstanceOf[Access]) {
              doInvokeSpecial = true
              val sType = getTypeWithAccess(access1.expression.asInstanceOf[Access], imports)
              if (!sType.isAssignableFrom(scope.typeOf.orNull))
                throw new SyntaxException(
                  "invokespecial type should be assignable from current class",
                  access1.lineCol)
              findMethodFromTypeWithArguements(
                access.lineCol,
                access.name,
                argList,
                if (scope.aThis == null) scope.typeOf.orNull else scope.aThis.typeOf(),
                sType,
                SemanticProcessor.FIND_MODE_NON_STATIC,
                methodsToInvoke,
                checkSuper = false
              )
            } else
              throw new SyntaxException(
                "`Type` in Type.this.methodName should be Class/Interface name",
                access1.lineCol)
          case _ =>
            if (!access.expression.isInstanceOf[PackageRef]) {
              var isValue = true
              var throwableWhenTryValue: Throwable = null
              try {
                target = parseValueFromExpression(access.expression, null, scope)
              } catch {
                case e: LtBug =>
                  isValue = false
                  throwableWhenTryValue = e
                case e: SyntaxException =>
                  isValue = false
                  throwableWhenTryValue = e
              }
              if (target == null) isValue = false
              else if (isGetFieldAtRuntime(target)) {
                access.expression match {
                  case access1: Access =>
                    try {
                      getTypeWithAccess(access1, imports)
                      isValue = false
                    } catch {
                      case _: SyntaxException | _: AssertionError =>
                    }
                  case _ =>
                }
              }
              if (isValue) {
                if (target.typeOf().isInstanceOf[SClassDef] || target.isInstanceOf[SInterfaceDef]) {
                  findMethodFromTypeWithArguements(
                    access.lineCol,
                    access.name,
                    argList,
                    target.typeOf(),
                    target.typeOf(),
                    SemanticProcessor.FIND_MODE_NON_STATIC,
                    methodsToInvoke,
                    checkSuper = true
                  )
                } else
                  target.typeOf() match {
                    case _: SAnnoDef =>
                      if (argList.nonEmpty)
                        throw new SyntaxException(
                          "Annotation don't have methods with non zero parameters",
                          access.expression.lineCol)
                      findMethodFromTypeWithArguements(
                        access.lineCol,
                        access.name,
                        argList,
                        target.typeOf(),
                        target.typeOf(),
                        SemanticProcessor.FIND_MODE_NON_STATIC,
                        methodsToInvoke,
                        checkSuper = true
                      )
                      if (methodsToInvoke.isEmpty)
                        throw new SyntaxException(
                          s"cannot find ${access.name} in ${target.typeOf()}",
                          access.expression.lineCol)
                    case _: PrimitiveTypeDef =>
                      target = boxPrimitive(target, access.expression.lineCol)
                      findMethodFromTypeWithArguements(
                        access.lineCol,
                        access.name,
                        argList,
                        target.typeOf(),
                        target.typeOf(),
                        SemanticProcessor.FIND_MODE_NON_STATIC,
                        methodsToInvoke,
                        checkSuper = true
                      )
                    case _ =>
                      if (access.expression.isInstanceOf[Access]) {
                        findMethodFromTypeWithArguements(
                          access.lineCol,
                          access.name,
                          argList,
                          scope.typeOf.orNull,
                          target.typeOf(),
                          SemanticProcessor.FIND_MODE_STATIC,
                          methodsToInvoke,
                          checkSuper = true
                        )
                        if (methodsToInvoke.isEmpty)
                          throw new SyntaxException(s"cannot find static method $exp", exp.lineCol)
                      } else {
                        if (throwableWhenTryValue == null) {
                          throw new SyntaxException(
                            s"method access structure should only be (type,methodName)/((type or null,this)," +
                              s"methodName)/(null,methodName)/(value,methodName)but got ${exp.access}",
                            access.expression.lineCol
                          )
                        } else
                          throw new SyntaxException(throwableWhenTryValue.getMessage, exp.lineCol)
                      }
                  }
              }

            }
        }
      }
    }
    if (methodsToInvoke.isEmpty && innerMethod == null) {
      var sType: STypeDef = null
      try {
        sType = getTypeWithAccess(access, imports)
      } catch {
        case _: SyntaxException | _: AssertionError | _: ClassCastException =>
      }
      sType match {
        case classDef: SClassDef =>
          for (con <- classDef.constructors) {
            val innerLoop = new Breaks
            innerLoop.breakable {
              val params = con.parameters
              if (argList.size == params.size) {
                for (i <- argList.indices) {
                  var v = argList(i)
                  val param = params(i)
                  if (!param.sType.isAssignableFrom(v.typeOf())) {
                    if (!param.sType.isInstanceOf[PrimitiveTypeDef]
                      && v.typeOf().isInstanceOf[PrimitiveTypeDef]) {
                      v = boxPrimitive(v, LineCol.SYNTHETIC)
                      if (!param.sType.isAssignableFrom(v.typeOf())) {
                        innerLoop.break()
                      }
                    } else {
                      innerLoop.break()
                    }
                  }
                }
                val aNew = New(con, exp.lineCol)
                //todo
//                argList = castArgsForMethodInvoke(argList, con.parameters, exp.lineCol)
                aNew.args ++= argList
                return aNew
              }
            }
          }

        case _ =>
      }
    }
    if (target == null) target = scope.aThis
    if (methodsToInvoke.isEmpty && innerMethod == null) {
      if (target == null) {
        if (scope.aThis == null)
          throw new SyntaxException("invoke dynamic only perform on instances", exp.lineCol)
        else {
          argList.+=:(scope.aThis)
        }
      } else {
        argList.+=:(target)
      }
      InvokeDynamic(
        getInvokeDynamicBootstrapMethod(),
        access.name,
        argList,
        getTypeWithName("java.lang.Object", exp.lineCol),
        Dynamic.INVOKE_STATIC,
        exp.lineCol
      )
    }
    null
  }

  def parseInsFromVariableDef(exp: VariableDef, scope: SemanticScope): Value = {
    val imports = fileNameToImport(exp.lineCol.fileName)
    val sType =
      if (exp.vType == null)
        getTypeWithName("java.lang.Object", exp.lineCol)
      else
        getTypeWithAccess(exp.vType, imports)

    val field = findFieldFromTypeDef(
      exp.name,
      scope.typeOf.orNull,
      scope.typeOf.orNull,
      if (scope.aThis == null) SemanticProcessor.FIND_MODE_STATIC
      else SemanticProcessor.FIND_MODE_ANY,
      checkSuper = false
    )

    if (field == null) {
      if (scope.getLeftValue(exp.name) == null) {
        val canChange = !exp.modifiers.exists(_.modifier == "val")
        val localVariable = LocalVariable(sType, canChange)
        scope.putLeftValue(exp.name, localVariable)
      } else
        throw new SyntaxException(s"${exp.name} is already defined", exp.lineCol)
    }

    if (exp.init != null) {
      val v = parseValueFromExpression(
        exp.init,
        sType,
        scope
      )

      val pack = ValuePack()
      if (field != null) {
        if (field.modifiers.contains(SModifier.STATIC)) {
          pack.instructions += PutStatic(field, v, exp.lineCol)
          val getStatic = GetStatic(field, exp.lineCol)
          pack.instructions += getStatic
        } else {
          pack.instructions += PutField(field, scope.aThis, v, exp.lineCol)
          val getField = GetField(field, exp.lineCol, scope.aThis)
          pack.instructions += getField
        }
      } else {
        val localVariable = scope.getLeftValue(exp.name).asInstanceOf[LocalVariable]
        pack.instructions += new TStore(localVariable, v, scope, exp.lineCol)
        val tLoad = new TLoad(localVariable, scope, exp.lineCol)
        pack.instructions += tLoad
      }
      return pack
    }
    null
  }

  def findFieldFromTypeDef(
      fieldName: String,
      theType: STypeDef,
      sType: STypeDef,
      mode: Int,
      checkSuper: Boolean): SFieldDef =
    theType match {
      case s: SClassDef =>
        findFieldFromClassDef(fieldName, theType.asInstanceOf[SClassDef], sType, mode, checkSuper)
      case s: SInterfaceDef =>
        findFieldFromInterfaceDef(fieldName, theType.asInstanceOf[SInterfaceDef], checkSuper)
      case _ =>
        throw new LtBug(s"the type to get field from cannot be $theType")
    }

  def findFieldFromClassDef(
      fieldName: String,
      theClass: SClassDef,
      sType: STypeDef,
      mode: Int,
      checkSuper: Boolean): SFieldDef = {
    for (f <- theClass.fields) {
      val loop = new Breaks
      loop.breakable {
        if (mode == SemanticProcessor.FIND_MODE_STATIC)
          if (!f.modifiers.contains(SModifier.STATIC))
            loop.break()
          else if (mode == SemanticProcessor.FIND_MODE_NON_STATIC)
            if (f.modifiers.contains(SModifier.STATIC))
              loop.break()

        if (f.name == fieldName) {
          if (f.modifiers.contains(SModifier.PUBLIC)) return f
          else if (f.modifiers.contains(SModifier.PROTECTED)) {
            if (theClass.isAssignableFrom(sType)
              || theClass.pkg == sType.pkg)
              return f
          } else if (f.modifiers.contains(SModifier.PRIVATE)) {
            if (theClass == sType) return f
          } else if (theClass.pkg == sType.pkg) return f
        }
      }
    }

    if (checkSuper) {
      var f: SFieldDef = null
      if (theClass.parent != null) {
        f = findFieldFromClassDef(fieldName, theClass.parent, sType, mode, true)
      }
      if (null == f) {
        if (mode != SemanticProcessor.FIND_MODE_NON_STATIC) {
          for (i <- theClass.superInterfaces) {
            if (f != null) return f
            f = findFieldFromInterfaceDef(fieldName, i, true)
          }
        }
      }
    }
    null
  }

  def findFieldFromInterfaceDef(
      fieldName: String,
      theInterface: SInterfaceDef,
      checkSuper: Boolean): SFieldDef = {
    for (f <- theInterface.fields) {
      if (f.name == fieldName) return f
    }
    if (checkSuper) {
      var f: SFieldDef = null
      for (i <- theInterface.superInterfaces) {
        if (f != null) return f
        f = findFieldFromInterfaceDef(fieldName, i, true)
      }
    }
    null
  }
  def parseValueFromAccess(exp: Access, scope: SemanticScope): Value = ???

  def parseValueFromIndex(exp: Index, scope: SemanticScope): Value = ???

  def parseValueFromOneVarOp(exp: OneVariableOperation, scope: SemanticScope): Value = ???

  def parseValueFromTwoVarOp(exp: TwoVariableOperation, scope: SemanticScope): Value = ???

  def parseValueFromAssignment(exp: Assignment, scope: SemanticScope): Value = ???

  def invokeUndefinedGet(lineCol: LineCol): Value = ???

  def parseValueFromArrayExp(exp: ArrayExp, typeDef: STypeDef, scope: SemanticScope): Value = ???

  def parseValueFromMapExp(exp: MapExp, scope: SemanticScope): Value = ???

  def parseValueFromProcedure(exp: Procedure, typeDef: STypeDef, scope: SemanticScope): Value = ???

  def parseValueFromLambda(exp: Lambda, typeDef: STypeDef, scope: SemanticScope): Value = ???

  def cast(typeDef: STypeDef, v: Value, lineCol: LineCol): _root_.latte.Value = ???

  def parseValueFromExpression(init: Expression, typeDef: STypeDef, scope: SemanticScope): Value = {
    val imports = fileNameToImport(init.lineCol.fileName)
    var v: Value = null
    init match {
      case e: NumberLiteral =>
        try {
          if (isInt(typeDef, e, e.lineCol)) {
            val intValue = IntValue(e.literal.toInt)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return intValue
            else
              return boxPrimitive(intValue, e.lineCol)
          } else if (isLong(typeDef, e, e.lineCol)) {
            val longValue = LongValue(e.literal.toLong)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return longValue
            else
              return boxPrimitive(longValue, e.lineCol)
          } else if (isShort(typeDef, e, e.lineCol)) {
            val shortValue = ShortValue(e.literal.toShort)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return shortValue
            else
              return boxPrimitive(shortValue, e.lineCol)
          } else if (isByte(typeDef, e, e.lineCol)) {
            val byteValue = ByteValue(e.literal.toByte)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return byteValue
            else
              return boxPrimitive(byteValue, e.lineCol)
          } else if (isDouble(typeDef, e, e.lineCol)) {
            val doubleValue = DoubleValue(e.literal.toDouble)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return doubleValue
            else
              return boxPrimitive(doubleValue, e.lineCol)
          } else if (isFloat(typeDef, e, e.lineCol)) {
            val floatValue = FloatValue(e.literal.toFloat)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return floatValue
            else
              return boxPrimitive(floatValue, e.lineCol)
          } else
            throw new SyntaxException(s"$init cannot be converted into $typeDef", init.lineCol)
        } catch {
          case n: NumberFormatException =>
            throw new SyntaxException(s"$init is not valid $typeDef", init.lineCol)
        }
      case exp: BoolLiteral =>
        if (isBool(typeDef, exp.lineCol)) {
          val literal = exp.literal
          val b = BoolValue(literal match {
            case "true" => true
            case "false" => false
            case _ => throw new IllegalArgumentException(s"$literal for bool literal")
          })
          if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
            return b
          else
            return boxPrimitive(b, exp.lineCol)
        } else
          throw new SyntaxException(s"$exp cannot be converted into $typeDef", exp.lineCol)
      case s: StringLiteral =>
        var str = s.literal
        str = str.substring(1, str.length - 1)
        str = unescape(str, s.lineCol)
        if (isChar(typeDef, s, s.lineCol)) {
          if (str.length == 1) {
            val charValue = CharValue(str.head)
            if (typeDef == null || typeDef.isInstanceOf[PrimitiveTypeDef])
              return charValue
            else
              boxPrimitive(charValue, s.lineCol)
          } else
            throw new SyntaxException(
              s"$s cannot be converted into char, char must hold one character",
              s.lineCol)
        } else if (typeDef == null || typeDef.isAssignableFrom(
            getTypeWithName("java.lang.String", s.lineCol))) {
          val stringConstantValue = StringConstantValue(str)
          stringConstantValue.sType =
            getTypeWithName("java.lang.String", s.lineCol).asInstanceOf[SClassDef]
          return stringConstantValue
        } else
          throw new SyntaxException(s"$s cannot be converted into $typeDef", s.lineCol)
      case exp: VariableDef =>
        return parseInsFromVariableDef(exp, scope)
      case exp: Invocation =>
        v = parseValueFromInvocation(exp, scope)
      case exp: AsType =>
        v = parseValueFromExpression(exp.exp, getTypeWithAccess(exp.access, imports), scope)
      case exp: Access =>
        if (exp.name == null)
          v = parseValueFromExpression(exp, typeDef, scope)
        else
          v = parseValueFromAccess(exp, scope)
      case exp: Index =>
        v = parseValueFromIndex(exp, scope)
      case exp: OneVariableOperation =>
        v = parseValueFromOneVarOp(exp, scope)
      case exp: TwoVariableOperation =>
        v = parseValueFromTwoVarOp(exp, scope)
      case exp: Assignment =>
        v = parseValueFromAssignment(exp, scope)
      case exp: UndefinedExp =>
        v = invokeUndefinedGet(exp.lineCol)
      case exp: Null =>
        v = NullValue.get()
      case exp: ArrayExp =>
        v = parseValueFromArrayExp(exp, typeDef, scope)
      case exp: MapExp =>
        v = parseValueFromMapExp(exp, scope)
      case exp: Procedure =>
        v = parseValueFromProcedure(exp, typeDef, scope)
      case exp: Lambda =>
        v = parseValueFromLambda(exp, typeDef, scope)
      case exp: TypeOf =>
        v = GetClass(
          getTypeWithAccess(exp.access, imports),
          getTypeWithName("java.lang.Class", LineCol.SYNTHETIC)
        )
      case _ =>
        throw new LtBug(s"unknown expression $init")
    }
    cast(typeDef, v, init.lineCol)
  }

  def parseField(
      variableDef: VariableDef,
      sClassDef: STypeDef,
      imports: ListBuffer[Import],
      mode: Int,
      bool: Boolean): Unit = ???

  def parseMethod(
      stmt: MethodStatement,
      i: Int,
      sClassDef: STypeDef,
      lastMethod: SMethodDef,
      imports: ListBuffer[Import],
      mode: Int,
      isStatic: Boolean): Unit = {
    val methodDef = SMethodDef(stmt.lineCol)
    methodDef.name = stmt.name
    methodDef.declaringType = sClassDef
    methodDef.returnType =
      if (stmt.returnType == null)
        getTypeWithName("java.lang.Object", stmt.lineCol)
      else
        getTypeWithAccess(stmt.returnType, imports)
    parseParameters(stmt.params.toList, i, methodDef, imports, allowAccessModifier = false)
    var hasAccessModifier =
      stmt.modifiers.exists(
        x =>
          x.modifier == "pub"
            || x.modifier == "pri"
            || x.modifier == "pkg"
            || x.modifier == "pro")
    if (hasAccessModifier)
      methodDef.modifiers += SModifier.PUBLIC

    def sModifier(m: Modifier, mode: Int, stmt: MethodStatement): Option[SModifier] =
      m.modifier match {
        case "pub" => Some(SModifier.PUBLIC)
        case "pri" =>
          if (mode == SemanticProcessor.PARSING_INTERFACE) None else Some(SModifier.PRIVATE)
        case "pro" =>
          if (mode == SemanticProcessor.PARSING_INTERFACE) None else Some(SModifier.PROTECTED)
        case "pkg" if mode == SemanticProcessor.PARSING_INTERFACE => None
        case "val" => Some(SModifier.FINAL)
        case "abs" => if (stmt.body.nonEmpty) None else Some(SModifier.ABSTRACT)
        case _ => None
      }

    stmt.modifiers.foreach { m =>
      sModifier(m, mode, stmt) match {
        case Some(x) => methodDef.modifiers += x
        case None => throw new SyntaxException("invalid modifier", m.lineCol)
      }
    }
    if (isStatic)
      methodDef.modifiers += SModifier.STATIC
    if (mode == SemanticProcessor.PARSING_INTERFACE
      && !methodDef.modifiers.contains(SModifier.ABSTRACT)
      && stmt.body.isEmpty) {
      methodDef.modifiers += SModifier.ABSTRACT
    }

    parseAnnos(stmt.annos, methodDef, imports, ElementType.METHOD)

    val methods: ListBuffer[SMethodDef] = ListBuffer()

    (mode, sClassDef) match {
      case (SemanticProcessor.PARSING_CLASS, t: SClassDef) =>
        methods ++= t.methods
      case (SemanticProcessor.PARSING_INTERFACE, t: SInterfaceDef) =>
        methods ++= t.methods
      case _ =>
        throw new LtBug(s"invalid mode $mode")
    }

    for (builtMethod <- methods) {
      if (builtMethod.name == methodDef.name) {
        if (builtMethod.parameters.size == methodDef.parameters.size) {
          val size = methodDef.parameters.size
          val builtParam = builtMethod.parameters
          val current = methodDef.parameters
          var passCheck = false
          for (in <- 0 until size) {
            if (builtParam(i).typeOf() != current(in).typeOf())
              passCheck = true
          }
          if (!passCheck)
            throw new SyntaxException(s"method signatrue check failed on $methodDef", stmt.lineCol)
        }
      }
    }

    if (lastMethod != null) {
      var invoke: InvokeWithTarget = null
      if (lastMethod.modifiers.contains(SModifier.PRIVATE))
        invoke = InvokeSpecial(This(methodDef.declaringType), lastMethod, LineCol.SYNTHETIC)
      else
        invoke = InvokeVirtual(This(methodDef.declaringType), lastMethod, LineCol.SYNTHETIC)

      invoke.arguments ++= methodDef.parameters

      val lastParams = lastMethod.parameters
      invoke.arguments += parseValueFromExpression(
        stmt.params(i).init,
        lastParams.last.typeOf(),
        null
      )
      methodDef.statements += invoke
    }

    (mode, sClassDef) match {
      case (SemanticProcessor.PARSING_CLASS, t: SClassDef) =>
        t.methods += methodDef
      case (SemanticProcessor.PARSING_INTERFACE, t: SInterfaceDef) =>
        t.methods += methodDef
      case _ =>
    }
  }

  def checkInterfaceCircularInheritance(
      i: SInterfaceDef,
      superInterfaces: ListBuffer[SInterfaceDef],
      buffer: ListBuffer[Nothing]): Unit = ???

  def checkOverride(s: STypeDef) = ???

  def parseValueFromObject(o: AnyRef) = ???

  def parseAnnoValues(annos: ListBuffer[SAnno]): Unit =
    for (sAnno <- annos) {
      val anno = annotationRecorder(sAnno)
      val map = mutable.HashMap[SAnnoField, Value]()
      for (f <- sAnno.typeOf().annoFields) {
        val innerLoop = new Breaks
        innerLoop.breakable {
          if (anno == null) {
            for (e <- sAnno.alreadyCompiledAnnotationValueMap) {
              if (e._1 == f.name) {
                val v = parseValueFromObject(e._2)
                map += f -> v
                innerLoop.break()
              }
            }
          } else {
            for (a <- anno.args) {
              if (a.assignTo.name == f.name) {
                val v = parseValueFromExpression(a.assignFrom, f.sType, null)
                map += f -> v
                innerLoop.break()
              }
            }
          }
        }
        if (f.defaultValue != null)
          map += f -> f.defaultValue
        else
          throw new SyntaxException(
            f.name +
              s" missing",
            if (anno == null) LineCol.SYNTHETIC else anno.lineCol)
      }
      sAnno.valueMap ++= map
    }

  private def isInt(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[IntTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Integer", lineCol)
      ) && !literal.literal.contains(".")))

  private def isLong(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[LongTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Long", lineCol)
      ) && !literal.literal.contains(".")))

  private def isShort(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[ShortTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Short", lineCol)
      ) && !literal.literal.contains(".")))

  private def isByte(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[ByteTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Byte", lineCol)
      ) && !literal.literal.contains(".")))

  private def isFloat(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[FloatTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Float", lineCol)
      )))

  private def isDouble(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[DoubleTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Double", lineCol)
      )))

  private def isBool(requiredType: STypeDef, lineCol: LineCol): Boolean =
    (requiredType == null || requiredType.isInstanceOf[BoolTypeDef]
      || (requiredType.isInstanceOf[SClassDef] && requiredType.isAssignableFrom(
        getTypeWithName("java.lang.Boolean", lineCol)
      )))

  private def isChar(requiredType: STypeDef, literal: StringLiteral, lineCol: LineCol): Boolean = {
    requiredType match {
      case null =>
        isChar(literal, lineCol, testSymbol = true)
      case _: CharTypeDef =>
        isChar(literal, lineCol, testSymbol = false)
      case _: SClassDef =>
        val characterClass = getTypeWithName("java.lang.Character", lineCol)
        if (requiredType == characterClass)
          return true
        if (requiredType.isAssignableFrom(characterClass)) {
          val stringClass = getTypeWithName("java.lang.String", lineCol)
          if (requiredType.isAssignableFrom(stringClass)) {
            return isChar(literal, lineCol, testSymbol = true)
          } else {
            return isChar(literal, lineCol, testSymbol = false)
          }
        }
      case _ => return false
    }
    false
  }

  private def isChar(literal: StringLiteral, lineCol: LineCol, testSymbol: Boolean): Boolean = {
    var str = literal.literal
    str = str.substring(1)
    str = str.substring(0, str.length - 1)
    if (testSymbol && !literal.literal.startsWith("\'"))
      return false
    unescape(str, lineCol).length == 1
  }

  private def unescape(str: String, col: LineCol): String = {
    val preResult = Array.ofDim[Char](str.length)
    var j = 0
    var i = 0
    while (j < str.toCharArray.length) {
      val c = str.charAt(i)
      if (c == '\\') {
        j += 1
        val anotherChar = str.charAt(j)
        c match {
          case 'n' => preResult(i) = '\n'
          case 'r' => preResult(i) = '\r'
          case '\'' => preResult(i) = '\''
          case '\\' => preResult(i) = '\\'
          case '\"' => preResult(i) = '\"'
          case 't' => preResult(i) = '\t'
          case _ =>
            throw new SyntaxException(s"cannot unescape \\ $anotherChar", col)
        }
      } else {
        preResult(j) = c
      }
      j += 1
      i += 1
    }
    val result = Array.ofDim[Char](i)
    Array.copy(preResult, 0, result, 0, i)
    new String(result)
  }

  def parseInstructionFromReturn(
      r: Return,
      returnType: STypeDef,
      scope: SemanticScope,
      instructions: ListBuffer[Instruction]): Unit = {
    var tReturn: TReturn = null
    if (r.exp == null)
      tReturn = TReturn(null, TReturn.Return, r.lineCol)
    else {
      val v = parseValueFromExpression(r.exp, returnType, scope)
      val sType = v.typeOf()

      def insType(t: STypeDef) =
        t match {
          case i
              if (i == IntTypeDef.get()
                || i == ShortTypeDef.get()
                || i == ByteTypeDef.get()
                || i == BoolTypeDef.get()
                || i == CharTypeDef.get()) =>
            TReturn.IReturn
          case i if i == LongTypeDef.get() =>
            TReturn.LReturn
          case i if i == FloatTypeDef.get() =>
            TReturn.FReturn
          case i if i == DoubleTypeDef.get() =>
            TReturn.DReturn
          case _ =>
            TReturn.AReturn
        }

      val ins = insType(sType)
      tReturn = TReturn(v, ins, r.lineCol)
    }
    instructions += tReturn
  }

  def parseInstructionFromIf(
      e: IfStatement,
      returnType: STypeDef,
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction],
      exceptionTables: ListBuffer[ExceptionTable]): Unit = ???

  def parseInstructionFromWhile(
      e: WhileStatement,
      returnType: STypeDef,
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction],
      exceptionTables: ListBuffer[ExceptionTable]): Unit = ???

  def parseInstructionFromFor(
      e: ForStatement,
      returnType: STypeDef,
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction],
      exceptionTables: ListBuffer[ExceptionTable]): Unit = ???

  def parseInstructionFromTry(
      e: Try,
      returnType: STypeDef,
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction],
      exceptionTables: ListBuffer[ExceptionTable]): Unit = ???

  def parseInnerMethod(e: MethodStatement, scope: SemanticScope): SMethodDef = {
    if (scope.parent == null)
      throw new LtBug("scope.parent should not be null.")
    if (scope.innerMethodMap.contains(e.name))
      throw new SyntaxException("duplicate inner method name", e.lineCol)
    if (e.modifiers.nonEmpty)
      throw new SyntaxException("inner method cannot have modifiers", e.lineCol)
    if (e.annos.nonEmpty)
      throw new SyntaxException("inner method cannot have annotations", e.lineCol)

    for (v <- e.params) {
      if (scope.getLeftValue(v.name).orNull != null)
        throw new SyntaxException(s"${v.name} is already used.", v.lineCol)
      if (v.init != null)
        throw new SyntaxException("parametes of inner methods cannot have default value", v.lineCol)
    }

    val methods =
      scope.typeOf.orNull match {
        case s: SClassDef => s.methods
        case s: SInterfaceDef => s.methods
      }

    var generatedMethodName = e.name + "$Latte$InnerMethod"
    var enable = true
    var i = 0
    while (enable) {
      val innerLoop = new Breaks
      innerLoop.breakable {
        val tmpName = generatedMethodName + i
        for (m <- methods) {
          if (m.name == tmpName) {
            i += 1
            innerLoop.break()
          }
        }
      }
      enable = false
    }
    generatedMethodName += i
    val name = e.name
    var paramCount = e.params.size
    val localVariable = scope.getLocalVariables
    val param4Locals = ListBuffer[VariableDef]()
    localVariable.foreach { x =>
      val k = x._1
      val v = x._2
      val variable = VariableDef(
        k,
        Set(),
        Access(
          PackageRef(v.pkg, LineCol.SYNTHETIC),
          if (v.fullName.contains("."))
            v.fullName.substring(v.fullName.lastIndexOf('.') + 1)
          else
            v.fullName,
          LineCol.SYNTHETIC
        ),
        null,
        Set(),
        LineCol.SYNTHETIC
      )
      param4Locals += variable
    }

    val newMethodDef = MethodStatement(
      generatedMethodName,
      Set(),
      e.returnType,
      e.params,
      Set(),
      e.body,
      e.lineCol
    )
    newMethodDef.params ++= param4Locals

    parseMethod(
      newMethodDef,
      newMethodDef.params.size,
      scope.typeOf.orNull,
      null,
      fileNameToImport(newMethodDef.lineCol.fileName),
      scope.typeOf.orNull match {
        case _: SClassDef => SemanticProcessor.PARSING_CLASS
        case _ => SemanticProcessor.PARSING_INTERFACE
      },
      scope.aThis == null
    )
    val m = methods.last
    m.modifiers -= SModifier.PUBLIC
    m.modifiers -= SModifier.PROTECTED
    m.modifiers.reverse.+=(SModifier.PRIVATE)
    m.modifiers.reverse

    scope.addMethodRef(name, MethodRecorder(m, paramCount))
    parseMethod1(m, newMethodDef.body, scope.parent)
    m
  }

  def parseStatement(
      statement: Statement,
      returnType: STypeDef,
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction],
      exceptionTables: ListBuffer[ExceptionTable],
      dontParseMethod: Boolean): Unit =
    statement match {
      case e: Expression =>
        val v = parseValueFromExpression(e, null, constructorScope)
        v match {
          case instruction: Instruction => instructions += instruction
          case _ =>
        }
      case r: Return =>
        parseInstructionFromReturn(r, returnType, constructorScope, instructions)
      case e: IfStatement =>
        parseInstructionFromIf(e, returnType, constructorScope, instructions, exceptionTables)
      case e: WhileStatement =>
        parseInstructionFromWhile(e, returnType, constructorScope, instructions, exceptionTables)
      case e: ForStatement =>
        parseInstructionFromFor(e, returnType, constructorScope, instructions, exceptionTables)
      case e: Throw =>
        var throwable = parseValueFromExpression(e.expression, null, constructorScope)
        if (!getTypeWithName("java.lang.Throwable", LineCol.SYNTHETIC)
            .isAssignableFrom(throwable.typeOf())) {
          val invokeStatic = InvokeStatic(langCastToThrowable, LineCol.SYNTHETIC)
          invokeStatic.arguments += throwable
          throwable = invokeStatic
        }
        val aThrow = AThrow(throwable, statement.lineCol)
        instructions += aThrow
      case e: Try =>
        parseInstructionFromTry(e, returnType, constructorScope, instructions, exceptionTables)
      case e: MethodStatement =>
        if (!dontParseMethod)
          parseInnerMethod(e, constructorScope)
      case _: StaticScope =>
        throw new LtBug(s"unknown statement $statement")
      case _: Pass =>
        throw new LtBug(s"unknown statement $statement")
      case _ =>
        throw new LtBug(s"unknown statement $statement")
    }

  def parseMethod1(
      m: SMethodDef,
      statements: ListBuffer[Statement],
      superScope: SemanticScope): Unit = {
    if (m.statements.nonEmpty || m.modifiers.contains(SModifier.ABSTRACT)) return
    val scope = new SemanticScope(superScope)
    if (!m.modifiers.contains(SModifier.STATIC))
      scope.aThis = This(scope.typeOf.orNull)
    m.parameters.foreach(p => scope.putLeftValue(p.name, p))
    statements.foreach(
      parseStatement(
        _,
        m.returnType,
        scope,
        m.statements,
        m.exceptionTables,
        dontParseMethod = false
      )
    )
  }

  def selectImportClassInterface(
      stmt: Statement,
      imports: ListBuffer[Import],
      classDefs: ListBuffer[ClassStatement],
      interfaceDefs: ListBuffer[InterfaceStatement]): Unit =
    stmt match {
      case i: Import =>
        imports += i
      case c: ClassStatement =>
        classDefs += c
      case i: InterfaceStatement =>
        interfaceDefs += i
      case _ =>
        throw UnexpectedTokenException(
          "class/interface definition or import",
          stmt.toString,
          stmt.lineCol)
    }

  def parse: mutable.HashSet[STypeDef] = {
    val fileNameToClassDef: mutable.HashMap[String, ListBuffer[ClassStatement]] = mutable.HashMap()
    val fileNameToInterfaceDef: mutable.HashMap[String, ListBuffer[InterfaceStatement]] =
      mutable.HashMap()
    val fileNameToPackageName: mutable.HashMap[String, String] = mutable.HashMap()
    mapOfStatements.foreach { x =>
      val imports: ListBuffer[Import] = ListBuffer()
      val classDefs: ListBuffer[ClassStatement] = ListBuffer()
      val interfaceDefs: ListBuffer[InterfaceStatement] = ListBuffer()
      val statements = x._2
      var pkg: String = ""
      val loop = new Breaks
      loop.breakable {

        fileNameToImport += x._1 -> imports
        fileNameToClassDef += x._1 -> classDefs
        fileNameToInterfaceDef += x._1 -> interfaceDefs

        val statementIterator = statements.toIterator
        if (statementIterator.hasNext) {
          val stmt = statementIterator.next()
          stmt match {
            case p: PackageDeclare =>
              pkg = p.pkg.pkg.replace("::", ".") + "."
            case _ =>
              selectImportClassInterface(stmt, imports, classDefs, interfaceDefs)
          }
          while (statementIterator.hasNext) {
            val stmt = statementIterator.next()
            selectImportClassInterface(stmt, imports, classDefs, interfaceDefs)
          }
        } else {
          loop.break()
        }
      }

      imports.+=:(
        Import(
          List(
            ImportDetail(
              PackageRef(
                if (pkg.endsWith("."))
                  pkg.substring(0, pkg.length - 1).replace(".", "::")
                else
                  pkg,
                LineCol.SYNTHETIC
              ),
              null,
              importAll = true)
          ),
          LineCol.SYNTHETIC
        )
      )

      imports += Import(
        List(
          ImportDetail(
            PackageRef("java::lang", LineCol.SYNTHETIC),
            null,
            importAll = true
          )),
        LineCol.SYNTHETIC
      )
      imports += Import(
        List(
          ImportDetail(
            PackageRef("lt::lang", LineCol.SYNTHETIC),
            null,
            importAll = true
          )),
        LineCol.SYNTHETIC
      )

      fileNameToPackageName += x._1 -> pkg

      val importSimpleNames: mutable.HashSet[String] = mutable.HashSet()
      imports.foreach { i =>
        i.importDetails.foreach { detail =>
          val className = getClassNameFromAccess(detail.access)
          if (!typeExists(className))
            throw new SyntaxException(className + "does not exist", i.lineCol)
          if (importSimpleNames.contains(detail.access.name))
            throw new SyntaxException("duplicate imports", i.lineCol)
          importSimpleNames += detail.access.name
        }
      }
      importSimpleNames.clear()

      classDefs.foreach { c =>
        val className = pkg + c.name
        if (types.contains(className))
          throw new SyntaxException(s"duplicate type names $className", c.lineCol)

        val sClassDef = SClassDef(c.lineCol)
        sClassDef.fullName = className
        sClassDef.pkg = if (pkg.endsWith(".")) pkg.substring(0, pkg.length - 1) else pkg
        sClassDef.modifiers += PUBLIC

        def getModifier(m: Modifier): Option[SModifier] =
          m.modifier match {
            case "abs" => Some(ABSTRACT)
            case "val" => Some(FINAL)
            case "pub" => None
            case "pri" => None
            case "pro" => None
            case "pkg" => None
            case _ =>
              throw UnexpectedTokenException(
                "valid modifier for class (val|abs)",
                m.toString,
                m.lineCol)
          }

        c.modifiers.map(getModifier).foreach {
          case Some(xx) => sClassDef.modifiers += xx
          case None =>
        }

        types += className -> sClassDef
        originalClasses += className -> c
        typeDefSet += sClassDef
      }

      interfaceDefs.foreach { i =>
        val interfaceName = pkg + i.name
        if (types.contains(interfaceName))
          throw new SyntaxException(s"duplicate type names $interfaceName", i.lineCol)

        val sInterfaceDef = SInterfaceDef(i.lineCol)
        sInterfaceDef.fullName = interfaceName
        sInterfaceDef.pkg = if (pkg.endsWith(".")) pkg.substring(0, pkg.length - 1) else pkg
        sInterfaceDef.modifiers += PUBLIC
        sInterfaceDef.modifiers += ABSTRACT

        if (i.modifiers.exists(_.modifier != "abs"))
          throw new UnexpectedTokenException(
            s"valid modifier for interface (abs) ${i.modifiers.toString()}",
            i.lineCol)

        types += interfaceName -> sInterfaceDef
        originalInterfaces += interfaceName -> i
        typeDefSet += sInterfaceDef
      }
    }

    mapOfStatements.foreach { m =>
      val imports: ListBuffer[Import] = fileNameToImport(m._1)
      val classDefs: ListBuffer[ClassStatement] = fileNameToClassDef(m._1)
      val interfaceDefs: ListBuffer[InterfaceStatement] = fileNameToInterfaceDef(m._1)
      var pkg: String = fileNameToPackageName(m._1)
      val loop = new Breaks
      loop.breakable {
        classDefs.foreach { c =>
          val sClassDef = types(pkg + c.name).asInstanceOf[SClassDef]
          var superWithoutInvocationAccess: Iterator[Access] = null
          if (c.superWithOutInvocation != null) {
            if (c.superWithOutInvocation.isEmpty) {
              sClassDef.parent =
                getTypeWithName("java.lang.Object", LineCol.SYNTHETIC).asInstanceOf[SClassDef]
            } else {
              superWithoutInvocationAccess = c.superWithOutInvocation.toIterator
              val mightBeClassAccess = superWithoutInvocationAccess.next()
              val tmp = getTypeWithAccess(mightBeClassAccess, imports)
              tmp match {
                case s: SClassDef =>
                  sClassDef.parent = s
                case s: SInterfaceDef =>
                  sClassDef.superInterfaces += s
                  sClassDef.parent =
                    getTypeWithName("java.lang.Object", c.lineCol).asInstanceOf[SClassDef]
                case _ =>
                  throw new SyntaxException(
                    mightBeClassAccess.toString + " is not class or interface",
                    mightBeClassAccess.lineCol)
              }
            }
          } else {
            val access = c.superWithInvocation.access
            val tmp = getTypeWithAccess(access, imports)
            tmp match {
              case s: SClassDef =>
                sClassDef.parent = s
              case _ =>
                throw new SyntaxException(
                  access.toString + "is not class or interface",
                  access.lineCol)
            }
            superWithoutInvocationAccess = c.superWithOutInvocation.iterator
          }

          while (superWithoutInvocationAccess != null && superWithoutInvocationAccess.hasNext) {
            val interfaceAccess = superWithoutInvocationAccess.next()
            val tmp = getTypeWithAccess(interfaceAccess, imports)
            tmp match {
              case s: SInterfaceDef =>
                sClassDef.superInterfaces += s
              case _ =>
                throw new SyntaxException(
                  interfaceAccess.toString + "is not interface",
                  interfaceAccess.lineCol)
            }
          }

          parseAnnos(c.annos, sClassDef, imports, ElementType.TYPE)

          var generateIndex = -1
          generateIndex = c.params.takeWhile(_.init == null).length

          var lastConstructor: SConstructorDef = null
          for (i <- c.params.size until generateIndex by -1) {
            val constructor = SConstructorDef(c.lineCol)

            constructor.declaringType = sClassDef

            var hasAccessModifier = false
            if (c.modifiers.exists(
                x =>
                  x.modifier == "pub"
                    || x.modifier == "pri"
                    || x.modifier == "pro"
                    || x.modifier == "pkg"))
              hasAccessModifier = true
            if (!hasAccessModifier)
              constructor.modifiers += SModifier.PUBLIC

            def getModifier(m: Modifier): Option[SModifier] =
              m.modifier match {
                case "abs" => None
                case "val" => None
                case "pub" => Some(PUBLIC)
                case "pri" => Some(SModifier.PRIVATE)
                case "pro" => Some(SModifier.PROTECTED)
                case "pkg" => None
                case _ =>
                  throw UnexpectedTokenException(
                    "valid modifier for class (val|abs)",
                    m.toString,
                    m.lineCol)
              }

            c.modifiers.foreach { mm =>
              getModifier(mm) match {
                case Some(x) => constructor.modifiers += x
                case None =>
              }
            }

            parseParameters(c.params, i, constructor, imports, allowAccessModifier = true)

            constructor.declaringType = sClassDef
            sClassDef.constructors += constructor
            if (lastConstructor != null) {
              val invoke = InvokeSpecial(This(sClassDef), lastConstructor, LineCol.SYNTHETIC)
              constructor.parameters.foreach(invoke.arguments += _)
              val paramOfLast = lastConstructor.parameters
              invoke.arguments += parseValueFromExpression(
                c.params(i).init,
                paramOfLast.last.typeOf(),
                null)
              constructor.statements += invoke
            }
            lastConstructor = constructor

          }

          c.params.foreach(
            parseField(_, sClassDef, imports, SemanticProcessor.PARSING_CLASS, bool = false))

          val staticScopes: ListBuffer[StaticScope] = ListBuffer()
          c.statements.foreach {
            case stmt: StaticScope =>
              staticScopes += stmt
            case stmt: VariableDef =>
              parseField(stmt, sClassDef, imports, SemanticProcessor.PARSING_CLASS, bool = false)
            case stmt: MethodStatement =>
              generateIndex = -1
              generateIndex = stmt.params.takeWhile(_.init == null).length
              var lastMethod: SMethodDef = null
              for (i <- stmt.params.size until generateIndex by -1) {
                parseMethod(
                  stmt,
                  i,
                  sClassDef,
                  lastMethod,
                  imports,
                  SemanticProcessor.PARSING_CLASS,
                  isStatic = false)
                lastMethod = sClassDef.methods.last
                methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
              }
            case _ =>
          }

          for (scope <- staticScopes) {
            scope.statements.foreach {
              case stmt: VariableDef =>
                parseField(stmt, sClassDef, imports, SemanticProcessor.PARSING_CLASS, bool = true)
              case stmt: MethodStatement =>
                generateIndex = -1
                generateIndex = stmt.params.takeWhile(_.init == null).length
                var lastMethod: SMethodDef = null
                for (i <- stmt.params.size until generateIndex by -1) {
                  parseMethod(
                    stmt,
                    i,
                    sClassDef,
                    lastMethod,
                    imports,
                    SemanticProcessor.PARSING_CLASS,
                    isStatic = true)
                  lastMethod = sClassDef.methods.last
                  methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
                }
              case _ =>
            }
          }
        }

        for (interfaceDef <- interfaceDefs) {
          val sInterfaceDef: SInterfaceDef =
            types(pkg + interfaceDef.name).asInstanceOf[SInterfaceDef]
          for (access <- interfaceDef.superInterfaces) {
            val superInterface = getTypeWithAccess(access, imports).asInstanceOf[SInterfaceDef]
            sInterfaceDef.superInterfaces += superInterface
          }

          parseAnnos(interfaceDef.annos, sInterfaceDef, imports, ElementType.TYPE)

          val staticScopes: ListBuffer[StaticScope] = ListBuffer()
          interfaceDef.statements.foreach {
            case stmt: StaticScope =>
              staticScopes += stmt
            case stmt: VariableDef =>
              parseField(
                stmt,
                sInterfaceDef,
                imports,
                SemanticProcessor.PARSING_INTERFACE,
                bool = false)
            case stmt: MethodStatement =>
              var generateIndex = -1
              generateIndex = stmt.params.takeWhile(_.init == null).length
              var lastMethod: SMethodDef = null
              for (i <- stmt.params.size until generateIndex by -1) {
                parseMethod(
                  stmt,
                  i,
                  sInterfaceDef,
                  lastMethod,
                  imports,
                  SemanticProcessor.PARSING_INTERFACE,
                  isStatic = false)
                lastMethod = sInterfaceDef.methods.last
                methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
              }
            case stmt @ _ =>
              throw new SyntaxException("interfaces don't have initiators", stmt.lineCol)
          }

          for (scope <- staticScopes) {
            scope.statements.foreach {
              case stmt: VariableDef =>
                parseField(
                  stmt,
                  sInterfaceDef,
                  imports,
                  SemanticProcessor.PARSING_INTERFACE,
                  bool = true)
              case stmt: MethodStatement =>
                var generateIndex = -1
                generateIndex = stmt.params.takeWhile(_.init == null).length
                var lastMethod: SMethodDef = null
                for (i <- stmt.params.size until generateIndex by -1) {
                  parseMethod(
                    stmt,
                    i,
                    sInterfaceDef,
                    lastMethod,
                    imports,
                    SemanticProcessor.PARSING_INTERFACE,
                    isStatic = true)
                  lastMethod = sInterfaceDef.methods.last
                  methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
                }
              case _ =>
            }
          }

        }
      }
    }
    typeDefSet.foreach {
      case sTypeDef: SClassDef =>
        val circularRecorder: ListBuffer[STypeDef] = ListBuffer()
        var parent = sTypeDef.parent
        while (parent != null) {
          circularRecorder += parent
          if (parent == sTypeDef)
            throw new SyntaxException("circular inheritance " + circularRecorder, LineCol.SYNTHETIC)
          parent = parent.parent
        }
      case i: SInterfaceDef =>
        checkInterfaceCircularInheritance(i, i.superInterfaces, ListBuffer())
      case t @ _ =>
        throw new IllegalArgumentException("wrong STypeDefType " + t.getClass)
    }

    for (s <- typeDefSet) {
      checkOverride(s)
      val loop = new Breaks
      loop.breakable {
        s match {
          case sc: SClassDef =>
            if (sc.modifiers.contains(SModifier.ABSTRACT))
              loop.break()
            var parent = sc.parent
            while (parent != null && parent.modifiers.contains(SModifier.ABSTRACT)) {
              if (parent.parent != null && parent.parent.modifiers.contains(SModifier.ABSTRACT))
                checkOverride(parent)
              parent = parent.parent
            }

            val queue = mutable.Queue[SInterfaceDef]()
            queue ++= sc.superInterfaces
            while (queue.nonEmpty) {
              val i = queue.dequeue()
              checkOverride(i)
              queue ++= i.superInterfaces
            }

            parent = sc.parent
            while (parent != null && parent.modifiers.contains(SModifier.ABSTRACT)) {
              for (m <- parent.methods) {
                if (m.modifiers.contains(SModifier.ABSTRACT)) {
                  if (m.overRidden.isEmpty)
                    throw new SyntaxException(m + s" is not overridden in $sc", sc.lineCol)
                }
              }
              parent = parent.parent
            }
            queue.clear()
            queue ++= sc.superInterfaces
            while (queue.nonEmpty) {
              val i = queue.dequeue()
              for (m <- i.methods) {
                if (m.modifiers.contains(SModifier.ABSTRACT)) {
                  if (m.overRidden.isEmpty)
                    throw new SyntaxException(m + s" is not overridden in $sc", sc.lineCol)
                }
              }
              queue ++= i.superInterfaces
            }
          case _ =>
        }
      }
    }
    types.values.foreach {
      case annoDef: SAnnoDef =>
        var cls: Class[_] = null
        try {
          cls = Class.forName(annoDef.fullName)
        } catch {
          case e: ClassNotFoundException =>
            throw new LtBug(e)
        }

        annoDef.annoFields.foreach { a =>
          try {
            val annoM = cls.getDeclaredMethod(a.name)
            try {
              val o = annoM.getDefaultValue
              if (o != null) {
                val value = parseValueFromObject(o)
                a.defaultValue = value
              }
            } catch {
              case _: TypeNotPresentException =>
            }
          } catch {
            case e: NoSuchMethodException =>
              throw new LtBug(e)
          }

        }
        parseAnnoValues(annoDef.annos)
    }

    for (sTypeDef <- typeDefSet) {
      sTypeDef match {
        case sClassDef: SClassDef =>
          val astClass = originalClasses(sClassDef.fullName)
          parseAnnoValues(sClassDef.annos)
          val scope = new SemanticScope(sTypeDef)

          var constructorToFillStatements: SConstructorDef = null
          sClassDef.constructors.foreach { cons =>
            if (cons.statements.isEmpty) constructorToFillStatements = cons
          }
          assert(constructorToFillStatements != null)
          val constructorScope = new SemanticScope(scope)
          constructorScope.aThis = This(sTypeDef)
          constructorToFillStatements.parameters.foreach(x =>
            constructorScope.putLeftValue(x.name, x))
          var parent = sClassDef.parent
          var invokeSpecial: InvokeSpecial = null
          if (astClass.superWithInvocation == null) {
            val loop = new Breaks
            loop.breakable {
              for (con <- parent.constructors) {
                if (con.parameters.isEmpty) {
                  invokeSpecial = InvokeSpecial(This(sClassDef), con, sClassDef.lineCol)
                  loop.break()
                }
              }
            }

          } else {
            for (cons <- parent.constructors) {
              val innerLoop = new Breaks
              innerLoop.breakable {
                if (cons.parameters.size == astClass.superWithInvocation.args.size) {
                  invokeSpecial =
                    InvokeSpecial(This(sClassDef), cons, astClass.superWithInvocation.lineCol)
                  val parameters = cons.parameters
                  val args = astClass.superWithInvocation.args
                  parameters.indices.foreach { i =>
                    val v = parseValueFromExpression(args(i), parameters(i).sType, constructorScope)
                    invokeSpecial.arguments += v

                  }
                  innerLoop.break()
                }
              }
            }
          }

          if (invokeSpecial == null)
            throw new SyntaxException(
              s"no suitable super constructor to invoke in $sClassDef",
              sClassDef.lineCol)
          constructorToFillStatements.statements += invokeSpecial

          val fieldLoop = new Breaks
          fieldLoop.breakable {
            for (param <- constructorToFillStatements.parameters) {
              val f: SFieldDef = sClassDef.fields.find(_.name == param.name).orNull
              if (f == null)
                throw new LtBug("f shouldn't be null")

              val putField = PutField(
                f,
                constructorScope.aThis,
                new TLoad(param, constructorScope, LineCol.SYNTHETIC),
                LineCol.SYNTHETIC)
              constructorToFillStatements.statements += putField
            }
          }

          astClass.statements.foreach(
            parseStatement(
              _,
              VoidType.get(),
              constructorScope,
              constructorToFillStatements.statements,
              constructorToFillStatements.exceptionTables,
              dontParseMethod = true
            )
          )

          for (m <- sClassDef.methods) {
            parseAnnoValues(m.annos)
            parseMethod1(m, methodToStatements(m), scope)
          }

          val staticScope = new SemanticScope(scope)

          for (statement <- astClass.statements) {
            if (statement.isInstanceOf[StaticScope]) {
              statement
                .asInstanceOf[StaticScope]
                .statements
                .foreach(
                  parseStatement(
                    _,
                    VoidType.get(),
                    staticScope,
                    sClassDef.staticStatements,
                    sClassDef.staticExceptionTable,
                    dontParseMethod = true
                  )
                )
            }
          }
        case sInterfaceDef: SInterfaceDef =>
          val astInterface = originalInterfaces(sInterfaceDef.fullName)
          parseAnnoValues(sInterfaceDef.annos)
          val scope = new SemanticScope(sInterfaceDef)
          sInterfaceDef.methods.foreach(x => parseMethod1(x, methodToStatements(x), scope))

          val staticScope = new SemanticScope(scope)
          astInterface.statements.foreach(
            parseStatement(
              _,
              VoidType.get(),
              staticScope,
              sInterfaceDef.staticStatements,
              sInterfaceDef.staticExceptionTable,
              dontParseMethod = true
            )
          )
        case _ =>
          throw new IllegalArgumentException("wrong STypeDefType " + sTypeDef.getClass)
      }
    }

    typeDefSet
  }

  def getTypeWithAccess(mightBeClassAccess: Access, imports: ListBuffer[Import]): STypeDef =
    null

  def getFieldsAndMethodsFromClass(
      cls: Class[_],
      declaringType: STypeDef,
      fields: ListBuffer[SFieldDef],
      methods: ListBuffer[SMethodDef]): Unit = {
    for (f <- cls.getDeclaredFields) {
      val fieldDef = SFieldDef(LineCol.SYNTHETIC)
      fieldDef.name = f.getName
      fieldDef.sType = getTypeWithName(f.getType.getName, LineCol.SYNTHETIC)
      getModifierFromMember(f, fieldDef)
      getAnnotationFromAnnotatedElement(f, fieldDef)
      fieldDef.declaringType = declaringType
      fields += fieldDef
    }
    for (m <- cls.getDeclaredMethods) {
      val methodDef = SMethodDef(LineCol.SYNTHETIC)
      methodDef.name = m.getName
      methodDef.declaringType = declaringType
      if (m.getReturnType == Void.TYPE)
        methodDef.returnType = VoidType.get()
      else
        methodDef.returnType = getTypeWithName(m.getReturnType.getName, LineCol.SYNTHETIC)

      getAnnotationFromAnnotatedElement(m, methodDef)
      getModifierFromMember(m, methodDef)
      getParameterFromClassArray(m.getParameterTypes, methodDef)
      methods += methodDef
    }
  }

  def getParameterFromClassArray(parameterTypes: Array[Class[_]], invokable: SInvokable): Unit =
    for (paramType <- parameterTypes) {
      val param = SParameter()
      param.name = "?"
      param.target = invokable
      param.sType = getTypeWithName(paramType.getName, LineCol.SYNTHETIC)
      getAnnotationFromAnnotatedElement(paramType, param)
      invokable.parameters += param
    }

  def getModifierFromMember(member: Member, sMember: SMember): Unit = {
    val ms = member.getModifiers
    val modifiers = sMember.modifiers
    if (java.lang.reflect.Modifier.isAbstract(ms))
      modifiers += SModifier.ABSTRACT
    if (java.lang.reflect.Modifier.isFinal(ms))
      modifiers += SModifier.FINAL
    if (java.lang.reflect.Modifier.isNative(ms))
      modifiers += SModifier.NATIVE
    if (java.lang.reflect.Modifier.isPrivate(ms))
      modifiers += SModifier.PRIVATE
    if (java.lang.reflect.Modifier.isProtected(ms))
      modifiers += SModifier.PROTECTED
    if (java.lang.reflect.Modifier.isPublic(ms))
      modifiers += SModifier.PUBLIC
    if (java.lang.reflect.Modifier.isStatic(ms))
      modifiers += SModifier.STATIC
    if (java.lang.reflect.Modifier.isStrict(ms))
      modifiers += SModifier.STRICT
    if (java.lang.reflect.Modifier.isSynchronized(ms))
      modifiers += SModifier.SYNCHRONIZED
    if (java.lang.reflect.Modifier.isTransient(ms))
      modifiers += SModifier.TRANSIENT
    if (java.lang.reflect.Modifier.isVolatile(ms))
      modifiers += SModifier.VOLATILE
  }

  def getTypeWithName(str: String, lineCol: LineCol): STypeDef =
    if (types.contains(str)) {
      types(str)
    } else {
      try {
        var cls = Class.forName(str)
        if (cls.isArray) {
          val name = cls.getName
          var dimension = 0
          while (cls.isArray) {
            dimension += 1
            cls = cls.getComponentType
          }
          val arrType = SArrayTypeDef()
          arrType.fullName = name
          putNameAndTypeDef(arrType, lineCol)
          arrType.dimension = dimension
          arrType.sType = getTypeWithName(cls.getName, lineCol)
          arrType
        } else {
          val modifiers: ListBuffer[SModifier] = ListBuffer()
          var typeDef: STypeDef = null
          if (cls.isAnnotation) {
            val a = SAnnoDef()
            a.fullName = str
            typeDef = a
            modifiers ++= a.modifiers
          } else if (cls.isInterface) {
            val i = SInterfaceDef(LineCol.SYNTHETIC)
            i.fullName = str
            typeDef = i
            modifiers ++= i.modifiers
          } else {
            val c = SClassDef(LineCol.SYNTHETIC)
            c.fullName = str
            modifiers ++= c.modifiers
          }
          if (cls.getPackage != null)
            typeDef.pkg = cls.getPackage.getName
          putNameAndTypeDef(typeDef, lineCol)
          getAnnotationFromAnnotatedElement(cls, typeDef)
          getModifierFromClass(cls, modifiers)

          typeDef match {
            case s: SInterfaceDef =>
              getSuperInterfaceFromClass(cls, s.superInterfaces)
              getFieldsAndMethodsFromClass(cls, s, s.fields, s.methods)
            case s: SClassDef =>
              getSuperInterfaceFromClass(cls, s.superInterfaces)
              if (cls != classOf[Object]) {
                s.parent =
                  getTypeWithName(cls.getSuperclass.getName, lineCol).asInstanceOf[SClassDef]
              }

              getFieldsAndMethodsFromClass(cls, s, s.fields, s.methods)

              for (con <- cls.getDeclaredConstructors) {
                val constructorDef = SConstructorDef(LineCol.SYNTHETIC)
                constructorDef.declaringType = s
                getAnnotationFromAnnotatedElement(con, constructorDef)
                getParameterFromClassArray(con.getParameterTypes, constructorDef)
                getModifierFromMember(con, constructorDef)
                s.constructors += constructorDef
              }
            case s: SAnnoDef =>
              for (m <- cls.getDeclaredMethods) {
                assert(m.getParameters.isEmpty)
                val annoField = new SAnnoField()
                annoField.name = m.getName
                annoField.sType = getTypeWithName(m.getReturnType.getName, lineCol)
                s.annoFields += annoField
              }
            case _ =>
          }
          typeDef
        }
      } catch {
        case _: Exception =>
          throw new SyntaxException("undefined class", lineCol)
      }
    }

  private def getAnnotationFromAnnotatedElement(
      elem: AnnotatedElement,
      presentable: SAnnotationPresentable): Unit =
    for (a <- elem.getAnnotations) {
      var aClass: Class[_] = a.getClass
      while (aClass != null && !aClass.isAnnotation && aClass.getInterfaces.nonEmpty) aClass =
        aClass.getInterfaces.head
      if (aClass != null && aClass.isAnnotation) {
        val sAnno = SAnno()
        sAnno.present = presentable
        sAnno.annoDef = getTypeWithName(aClass.getName, LineCol.SYNTHETIC).asInstanceOf[SAnnoDef]
        presentable.annos() += sAnno

        for (m <- aClass.getDeclaredMethods) {
          assert(m.getParameterCount == 0)
          try {
            sAnno.alreadyCompiledAnnotationValueMap += m.getName -> m.invoke(a)
          } catch {
            case e: Exception => throw new LtBug(e)
          }
        }
      }
    }

  private def getModifierFromClass(cls: Class[_], modifiers: ListBuffer[SModifier]): Unit = {
    val ms = cls.getModifiers
    if (java.lang.reflect.Modifier.isAbstract(ms))
      modifiers += SModifier.ABSTRACT
    if (java.lang.reflect.Modifier.isFinal(ms))
      modifiers += SModifier.FINAL
    if (java.lang.reflect.Modifier.isNative(ms))
      modifiers += SModifier.NATIVE
    if (java.lang.reflect.Modifier.isPrivate(ms))
      modifiers += SModifier.PRIVATE
    if (java.lang.reflect.Modifier.isProtected(ms))
      modifiers += SModifier.PROTECTED
    if (java.lang.reflect.Modifier.isPublic(ms))
      modifiers += SModifier.PUBLIC
    if (java.lang.reflect.Modifier.isStatic(ms))
      modifiers += SModifier.STATIC
    if (java.lang.reflect.Modifier.isStrict(ms))
      modifiers += SModifier.STRICT
    if (java.lang.reflect.Modifier.isSynchronized(ms))
      modifiers += SModifier.SYNCHRONIZED
    if (java.lang.reflect.Modifier.isTransient(ms))
      modifiers += SModifier.TRANSIENT
    if (java.lang.reflect.Modifier.isVolatile(ms))
      modifiers += SModifier.VOLATILE
  }

  private def getSuperInterfaceFromClass(
      cls: Class[_],
      superInterfaces: ListBuffer[SInterfaceDef]) =
    cls.getInterfaces
      .filter(!_.isAnnotation)
      .foreach(x =>
        superInterfaces += getTypeWithName(x.getName, LineCol.SYNTHETIC)
          .asInstanceOf[SInterfaceDef])

  private def putNameAndTypeDef(sTypeDef: STypeDef, lineCol: LineCol): Unit =
    if (types.contains(sTypeDef.fullName))
      throw new SyntaxException(s"duplicate type names ${sTypeDef.fullName}", lineCol)
    else
      types += sTypeDef.fullName -> sTypeDef

  def getClassNameFromAccess(access: Access): String = {
    var pre: String = ""
    access.expression match {
      case a: Access =>
        pre = getClassNameFromAccess(a.expression.asInstanceOf[Access]) + "."
      case p: PackageRef =>
        pre = p.pkg.replace("::", ".") + "."
      case _ =>
    }
    pre + access.name
  }

  def typeExists(sType: String): Boolean = {
    if (types.contains(sType)) {
      try {
        Class.forName(sType)
      } catch {
        case _: ClassNotFoundException =>
          return false
      }
    }
    true
  }

}

object SemanticProcessor {
  val PARSING_CLASS = 0
  val PARSING_INTERFACE = 1

  /**
    * search for static and non-static
    */
  private val FIND_MODE_ANY = 0

  /**
    * only search for static
    */
  private val FIND_MODE_STATIC = 1

  /**
    * only search for non-static
    */
  private val FIND_MODE_NON_STATIC = 2
}
