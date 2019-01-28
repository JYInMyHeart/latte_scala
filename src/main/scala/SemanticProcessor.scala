package latte

import java.lang.annotation.ElementType
import java.lang.reflect.{AnnotatedElement, Constructor}

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
      sClassDef: STypeDef,
      imports: ListBuffer[Import],
      TYPE: ElementType): Unit = ???

  def parseParameters(
      params: List[VariableDef],
      i: Int,
      constructor: SConstructorDef,
      imports: ListBuffer[Import],
      bool: Boolean): Unit = {}

  def parseValueFromExpression(init: Expression, typeDef: STypeDef, value: SemanticScope): Value =
    ???

  def parseField(
      variableDef: VariableDef,
      sClassDef: STypeDef,
      imports: ListBuffer[Import],
      PARSING_CLASS: Int,
      bool: Boolean): Unit = ???

  def parseMethod(
      stmt: MethodStatement,
      i: Int,
      sClassDef: STypeDef,
      lastMethod: SMethodDef,
      imports: ListBuffer[Import],
      PARSING_CLASS: Int,
      bool: Boolean) = ???

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

  private def isBool(requiredType: STypeDef, literal: NumberLiteral, lineCol: LineCol): Boolean =
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
      constructorScope: SemanticScope,
      instructions: ListBuffer[Instruction]): Unit = ???

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
            true
          )),
        LineCol.SYNTHETIC
      )
      imports += Import(
        List(
          ImportDetail(
            PackageRef("lt::lang", LineCol.SYNTHETIC),
            null,
            true
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

            parseParameters(c.params, i, constructor, imports, true)

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
            parseField(_, sClassDef, imports, SemanticProcessor.PARSING_CLASS, false))

          val staticScopes: ListBuffer[StaticScope] = ListBuffer()
          c.statements.foreach {
            case stmt: StaticScope =>
              staticScopes += stmt
            case stmt: VariableDef =>
              parseField(stmt, sClassDef, imports, SemanticProcessor.PARSING_CLASS, false)
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
                  false)
                lastMethod = sClassDef.methods.last
                methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
              }
            case _ =>
          }

          for (scope <- staticScopes) {
            scope.statements.foreach {
              case stmt: VariableDef =>
                parseField(stmt, sClassDef, imports, SemanticProcessor.PARSING_CLASS, true)
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
                    true)
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
              parseField(stmt, sInterfaceDef, imports, SemanticProcessor.PARSING_INTERFACE, false)
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
                  false)
                lastMethod = sInterfaceDef.methods.last
                methodToStatements += lastMethod -> ListBuffer(stmt.body: _*)
              }
            case stmt @ _ =>
              throw new SyntaxException("interfaces don't have initiators", stmt.lineCol)
          }

          for (scope <- staticScopes) {
            scope.statements.foreach {
              case stmt: VariableDef =>
                parseField(stmt, sInterfaceDef, imports, SemanticProcessor.PARSING_INTERFACE, true)
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
                    true)
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
          case sc: SClassDef => {
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

          }
          case _ =>
        }
      }
    }
    types.values.foreach {
      case annoDef: SAnnoDef => {
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
    }

    for (sTypeDef <- typeDefSet) {
      sTypeDef match {
        case sClassDef: SClassDef => {
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
        }
        case sInterfaceDef: SInterfaceDef => {
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
        }
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
      s: STypeDef,
      fields: ListBuffer[SFieldDef],
      methods: ListBuffer[SMethodDef]): Unit = ???

  def getParameterFromClassArray(
      getParameterTypes: Array[Class[_]],
      constructorDef: SConstructorDef): Unit = ???

  def getModifierFromMember(con: Constructor[_], constructorDef: SConstructorDef): Unit = ???

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
      presentable: SAnnotationPresentable): Unit = {}

  private def getModifierFromClass(cls: Class[_], modifiers: ListBuffer[SModifier]): Unit = {}

  private def getSuperInterfaceFromClass(
      cls: Class[_],
      superInterfaces: ListBuffer[SInterfaceDef]) = {}

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
}
