package latte

import java.io.{BufferedReader, StringReader}

import latte.ParserTest.parse

class ParserTest extends UnitSpec {

  "testOperatorInSamePriority" should "1+2-3+4" in {
    val statements = parse("1+2-3+4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("+", one, two, LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("-", tvo1, three, LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", tvo2, four, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }

  "testOperatorInDifferentPriorities" should "1+2*3+4" in {
    val statements = parse("1+2*3+4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", one, tvo1, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", tvo2, four, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }

  "test 1 plus 2" should "nice" in {
    val statements = parse("1*2")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", one, two, LineCol.SYNTHETIC)
    assert(tvo1 == statement)
  }

  "test1Plus2Multi3" should "nice" in {
    val statements = parse("1+2*3")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", one, tvo1, LineCol.SYNTHETIC)
    assert(tvo2 == statement)
  }


  "test1Plus2Multi3Div4" should "nice" in {
    val statements = parse("1+2*3/4")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("/", tvo1, four, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", one, tvo2, LineCol.SYNTHETIC)
    assert(tvo3 == statement)
  }


  "test1Plus2Multi3Div4Minus5" should "nice" in {
    val statements = parse("1+2*3/4-5")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val five = NumberLiteral("5", LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", two, three, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("/", tvo1, four, LineCol.SYNTHETIC)
    val tvo3 = TwoVariableOperation("+", one, tvo2, LineCol.SYNTHETIC)
    val tvo4 = TwoVariableOperation("-", tvo3, five, LineCol.SYNTHETIC)
    assert(tvo4 == statement)
  }


  "testPar1Plus2ParMulti3" should "nice" in {
    val statements = parse("(1+2)*3")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)

    val tvo1 = TwoVariableOperation("+", one, two, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("*", tvo1, three, LineCol.SYNTHETIC)

    assert(tvo2 == statement)
  }


  "testBinOperator" should "nice" in {
    val statements = parse("1*3/(4+5)*6-(7/8+9)-10-15")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val four = NumberLiteral("4", LineCol.SYNTHETIC)
    val five = NumberLiteral("5", LineCol.SYNTHETIC)
    val six = NumberLiteral("6", LineCol.SYNTHETIC)
    val seven = NumberLiteral("7", LineCol.SYNTHETIC)
    val eight = NumberLiteral("8", LineCol.SYNTHETIC)
    val nine = NumberLiteral("9", LineCol.SYNTHETIC)
    val ten = NumberLiteral("10", LineCol.SYNTHETIC)
    val fifteen = NumberLiteral("15", LineCol.SYNTHETIC)

    val oneMulThree = TwoVariableOperation("*", one, three, LineCol.SYNTHETIC)
    val fourPlusFive = TwoVariableOperation("+", four, five, LineCol.SYNTHETIC)
    val DIV1 = TwoVariableOperation("/", oneMulThree, fourPlusFive, LineCol.SYNTHETIC)
    val MUL1 = TwoVariableOperation("*", DIV1, six, LineCol.SYNTHETIC)
    val sevenDIVIDEeight = TwoVariableOperation("/", seven, eight, LineCol.SYNTHETIC)
    val DIVPLUSnine = TwoVariableOperation("+", sevenDIVIDEeight, nine, LineCol.SYNTHETIC)
    val MINUS1 = TwoVariableOperation("-", MUL1, DIVPLUSnine, LineCol.SYNTHETIC)
    val MINUS10 = TwoVariableOperation("-", MINUS1, ten, LineCol.SYNTHETIC)
    val MINUS15 = TwoVariableOperation("-", MINUS10, fifteen, LineCol.SYNTHETIC)
    assert(MINUS15 == statement)
  }

  "testOperators" should "nice" in {
    val statements = parse("+1++ -3^!true+2+\"abc\"")
    assert(1 == statements.size)
    val statement = statements.head
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val postPlusPlus = OneVariableOperation("++", one, LineCol.SYNTHETIC)
    val plusOne = UnaryOneVariableOperation("+", postPlusPlus, LineCol.SYNTHETIC)
    val three = NumberLiteral("3", LineCol.SYNTHETIC)
    val minus = TwoVariableOperation("-", plusOne, three, LineCol.SYNTHETIC)

    val tr = BoolLiteral("true", LineCol.SYNTHETIC)
    val not = UnaryOneVariableOperation("!", tr, LineCol.SYNTHETIC)

    val two = NumberLiteral("2", LineCol.SYNTHETIC)
    val plusTwo = TwoVariableOperation("+", not, two, LineCol.SYNTHETIC)
    val abc = StringLiteral("\"abc\"", LineCol.SYNTHETIC)
    val plusABC = TwoVariableOperation("+", plusTwo, abc, LineCol.SYNTHETIC)

    val xor = TwoVariableOperation("^", minus, plusABC, LineCol.SYNTHETIC)
    assert(xor == statement)
  }

  "testPost1VarWithOperatorPriority" should "nice" in {
    val statements = parse("1+1++ *1")
    assert(1 == statements.size)
    val statement = statements.head

    val n1 = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("1", LineCol.SYNTHETIC)
    val n3 = NumberLiteral("1", LineCol.SYNTHETIC)

    val ovo = OneVariableOperation("++", n2, LineCol.SYNTHETIC)
    val tvo1 = TwoVariableOperation("*", ovo, n3, LineCol.SYNTHETIC)
    val tvo2 = TwoVariableOperation("+", n1, tvo1, LineCol.SYNTHETIC)
    assert(tvo2 == statement)
  }


  "testPackage" should "nice" in {
    val statements = parse("java::lang::Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val a = Access(pkg, "Integer", LineCol.SYNTHETIC)
    assert(a == statement)
  }

  "testPkgAccess" should "nice" in {
    val statements = parse("java::lang::String.cls")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "String", LineCol.SYNTHETIC)
    val access2 = Access(access, "cls", LineCol.SYNTHETIC)
    assert(access2 == statement)
  }

  "testInvocation" should "nice" in {
    val statements = parse("java::lang::String.valueOf(true)")
    assert(1 == statements.size)
    val statement = statements.head
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "String", LineCol.SYNTHETIC)
    val access2 = Access(access, "valueOf", LineCol.SYNTHETIC)

    val invocation = Invocation(access2, List[Expression](BoolLiteral("true", LineCol.SYNTHETIC)), LineCol.SYNTHETIC)
    assert(statement == invocation)
  }

  "testInvocationNoArg" should "nice" in {
    val statements = parse("method()")
    assert(1 == statements.size)
    val statement = statements.head
    val invocation = Invocation(Access(null, "method", LineCol.SYNTHETIC), List(), LineCol.SYNTHETIC)
    assert(invocation == statement)
  }

  "testVariableWithInitValue" should "nice" in {
    val statements = parse("i=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_FullName" should "nice" in {
    val statements = parse("i:java::lang::Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName" should "nice" in {
    val statements = parse("i:Integer")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access = Access(null, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    assert(statement == v)
  }

  "testVariableWithInitType_FullName_Inner" should "nice" in {
    val statements = parse("i:mePackage::ClassName.Inner")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("mePackage", LineCol.SYNTHETIC)
    val access1 = Access(pkg, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner" should "nice" in {
    val statements = parse("i:ClassName.Inner")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    assert(statement == v)
  }


  "testVariableWithInitType_FullName_Init" should "nice" in {
    val statements = parse("i:java::lang::Integer=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("java::lang", LineCol.SYNTHETIC)
    val access = Access(pkg, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }


  "testVariableWithInitType_SimpleName_Init" should "nice" in {
    val statements = parse("i:Integer=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access = Access(null, "Integer", LineCol.SYNTHETIC)
    v.vType = access
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }


  "testVariableWithInitType_FullName_Inner_Init" should "nice" in {
    val statements = parse("i:mePackage::ClassName.Inner=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val pkg = PackageRef("mePackage", LineCol.SYNTHETIC)
    val access1 = Access(pkg, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner_Init" should "nice" in {
    val statements = parse("i:ClassName.Inner=1")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    v.init = n
    assert(statement == v)
  }

  "testVariableWithInitType_SimpleName_Inner_Init_Operator" should "nice" in {
    val statements = parse("i:ClassName.Inner=1+2")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("2", LineCol.SYNTHETIC)
    val o = TwoVariableOperation("+", n, n2, LineCol.SYNTHETIC)
    v.init = o
    assert(statement == v)
  }


  "testModifier" should "nice" in {
    val statements = parse("val i:ClassName.Inner=1+2")
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("i", Set(Modifier("val", LineCol.SYNTHETIC)), null, null, Set(), LineCol.SYNTHETIC)
    val access1 = Access(null, "ClassName", LineCol.SYNTHETIC)
    val access2 = Access(access1, "Inner", LineCol.SYNTHETIC)
    v.vType = access2
    val n = NumberLiteral("1", LineCol.SYNTHETIC)
    val n2 = NumberLiteral("2", LineCol.SYNTHETIC)
    val o = TwoVariableOperation("+", n, n2, LineCol.SYNTHETIC)
    v.init = o
    assert(statement == v)
  }


  "testAssign" should "nice" in {
    val statements = parse("i=1\ni=2")
    assert(2 == statements.size)
    val statement = statements(1)
    val access = Access(null, "i", LineCol.SYNTHETIC)
    val n = NumberLiteral("2", LineCol.SYNTHETIC)
    val ass = Assignment(access, "=", n, LineCol.SYNTHETIC)
    assert(ass == statement)
  }


  "testMethodNormal_Noparam" should "nice" in {
    val statements = parse(
      "" + "method()\n"
        + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = BoolLiteral("false", LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(v),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodType_NoParam" should "nice" in {
    val statements = parse(
      "" + "method():Integer\n"
        + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = BoolLiteral("false", LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(),
      Access(null, "Integer", LineCol.SYNTHETIC),
      List(),
      Set(),
      List(v),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testMethodType_NoParam_NoStmt" should "nice" in {
    val statements = parse(
      "" + "method():Integer"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      Access(null, "Integer", LineCol.SYNTHETIC),
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodEmpty_NoParam" should "nice" in {
    val statements = parse(
      "" + "method()=..."
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }

  "testMethodEmpty_Stmt_NoParam" should "nice" in {
    val statements = parse(
      "" + "method()=123"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val methodStatement = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(
        Return(
          NumberLiteral("123", LineCol.SYNTHETIC),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testMethodGeneral" should "nice" in {
    val statements = parse(
      "" + "abs method(a,b:Character):Integer\n" + "    a=false"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val a = VariableDef("a", Set(), null, null, Set(), LineCol.SYNTHETIC)
    val b = VariableDef("b", Set(), null, null, Set(), LineCol.SYNTHETIC)
    b.vType = Access(null, "Character", LineCol.SYNTHETIC)
    val vars = List(a, b)
    val assignment = Assignment(
      Access(null, "a", LineCol.SYNTHETIC),
      "=",
      BoolLiteral("false", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC)
    val methodStatement = MethodStatement(
      "method",
      Set(Modifier("abs", LineCol.SYNTHETIC)),
      Access(null, "Integer", LineCol.SYNTHETIC),
      vars,
      Set(),
      List(assignment),
      LineCol.SYNTHETIC
    )
    assert(methodStatement == statement)
  }


  "testReturn" should "nice" in {
    val statements = parse(
      "<i+1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val access = Access(null, "i", LineCol.SYNTHETIC)
    val one = NumberLiteral("1", LineCol.SYNTHETIC)
    val tvo = TwoVariableOperation("+", access, one, LineCol.SYNTHETIC)
    val ret = Return(tvo, LineCol.SYNTHETIC)
    assert(statement == ret)
  }

  "testReturnVoid" should "nice" in {
    val statements = parse(
      "<"
    )
    assert(1 == statements.size)
    val statement = statements.head
    assert(statement.asInstanceOf[Return].exp == null)
  }

  "testJavaName" should "nice" in {
    try {
      parse("sync=1")
    }
    catch {
      case _: Exception =>
    }
    val statements = parse(
      "`sync`=1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val v = VariableDef("sync", Set(), null, null, Set(), LineCol.SYNTHETIC)
    v.init = NumberLiteral("1", LineCol.SYNTHETIC)
    assert(v == statement)
  }

  "testIf" should "nice" in {
    val statements = parse(
      "if true"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val ifStatement = IfStatement(
      List(
        IfPair(BoolLiteral("true", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1"
    )
    assert(1 == statements.size)
    val statement = statements.head
    val ifStatement = IfStatement(
      List(
        IfPair(BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testIfBodyElseIf" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElse" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2\n" +
        "else\n"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElseBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval\n" +
        "    a=2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfBodyElseBody_1" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval1\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          Access(null, "boolval2", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testIfBodyElseIfBodyElseBody_2" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval1\n" +
        "    a=2\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("2", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        ),
        IfPair(
          Access(null, "boolval2", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }


  "testIfBodyElseIfElseBody" should "nice" in {
    val statements = parse(
      "if true\n" +
        "    a=1\n" +
        "elseif boolval2\n" +
        "else\n" +
        "    a=3"
    )
    assert(1 == statements.size)
    val statement = statements.head

    val ifStatement = IfStatement(
      List(
        IfPair(
          BoolLiteral("true", LineCol.SYNTHETIC),
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("1", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC),
        IfPair(
          Access(null, "boolval1", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ),
        IfPair(
          null,
          List(
            VariableDef("a",
              Set(),
              null,
              NumberLiteral("3", LineCol.SYNTHETIC),
              Set(),
              LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(ifStatement == statement)
  }

  "testFor1" should "nice" in {
    val statements = parse("for i in ite")
    assert(statements.size == 1)
    val s = statements.head
    val forStatement = ForStatement(
      "i",
      Access(null, "ite", LineCol.SYNTHETIC),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == forStatement)
  }

  "testFor2" should "nice" in {
    val statements = parse("for i in ite\n    a=1")
    assert(statements.size == 1)
    val s = statements.head
    val forStatement = ForStatement(
      "i",
      Access(null, "ite", LineCol.SYNTHETIC),
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == forStatement)
  }

  "testLambdaNoParam" should "nice" in {
    val statements = parse("()->1 + 1")
    assert(statements.size == 1)
    val s = statements.head
    val lambda = Lambda(
      List(),
      List(
        Return(
          TwoVariableOperation(
            "+",
            NumberLiteral("1", LineCol.SYNTHETIC),
            NumberLiteral("1", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == lambda)
  }

  "testLambda2Param" should "nice" in {
    val statements = parse("(a,b)->a+b")
    assert(statements.size == 1)
    val s = statements.head
    val lambda = Lambda(
      List(
        VariableDef("a",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC),
        VariableDef("b",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC)
      ),
      List(
        Return(
          TwoVariableOperation(
            "+",
            Access(null, "a", LineCol.SYNTHETIC),
            Access(null, "b", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == lambda)
  }

  "testLambdaInMethodInvocation" should "nice" in {
    val statements = parse("" + "method(\n" + "    (a)->a+1\n" + "    1" + ")")
    assert(statements.size == 1)
    val s = statements.head
    val invocation = Invocation(
      Access(null, "method", LineCol.SYNTHETIC),
      List(
        Lambda(
          List(
            VariableDef("a",
              Set(),
              null,
              null,
              Set(),
              LineCol.SYNTHETIC),
          ),
          List(
            Return(
              TwoVariableOperation(
                "+",
                Access(null, "a", LineCol.SYNTHETIC),
                NumberLiteral("1", LineCol.SYNTHETIC),
                LineCol.SYNTHETIC
              ),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        ),
        NumberLiteral("1", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )

    assert(s == invocation)
  }

  "testStatic1" should "nice" in {
    val statements = parse("static a=1")
    assert(statements.size == 1)
    val s = statements.head
    val ss = StaticScope(
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ss == s)
  }

  "testStatic2" should "nice" in {
    val statements = parse("static\n    a=1\n    b=2")
    assert(statements.size == 1)
    val s = statements.head
    val ss = StaticScope(
      List(
        VariableDef("a",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC),
        VariableDef("b",
          Set(),
          null,
          NumberLiteral("2", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(ss == s)
  }


  "testClass_Arg_SuperInvocation" should "nice" in {
    val statements = parse("class C(arg):Type(arg),Type2\n    a=2")
    assert(statements.size == 1)
    val s = statements.head
    val c = ClassStatement(
      "C",
      Set(),
      List(
        VariableDef("arg",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC)
      ),
      Invocation(
        Access(null, "Type", LineCol.SYNTHETIC),
        List(
          Access(null, "arg", LineCol.SYNTHETIC)
        ),
        LineCol.SYNTHETIC

      ),
      List(
        Access(null, "Type2", LineCol.SYNTHETIC)
      ),
      Set(),
      List(VariableDef("a",
        Set(),
        null,
        NumberLiteral("2", LineCol.SYNTHETIC),
        Set(),
        LineCol.SYNTHETIC)),
      LineCol.SYNTHETIC
    )
    assert(s == c)
  }


  "testClass_Arg_SuperInvocation_Modifiers" should "nice" in {
    val statements = parse("abs class C(arg):Type(arg),Type2\n    a=2")
    assert(statements.size == 1)
    val s = statements.head
    val c = ClassStatement(
      "C",
      Set(Modifier(
        "abs",
        LineCol.SYNTHETIC
      )),
      List(
        VariableDef("arg",
          Set(),
          null,
          null,
          Set(),
          LineCol.SYNTHETIC)
      ),
      Invocation(
        Access(null, "Type", LineCol.SYNTHETIC),
        List(
          Access(null, "arg", LineCol.SYNTHETIC)
        ),
        LineCol.SYNTHETIC

      ),
      List(
        Access(null, "Type2", LineCol.SYNTHETIC)
      ),
      Set(),
      List(VariableDef("a",
        Set(),
        null,
        NumberLiteral("2", LineCol.SYNTHETIC),
        Set(),
        LineCol.SYNTHETIC)),
      LineCol.SYNTHETIC
    )
    assert(s == c)
  }

  "testInterface_simple" should "nice" in {
    val statements = parse("interface A")
    assert(statements.size == 1)
    val s = statements.head

    val i = InterfaceStatement(
      "A",
      Set(),
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == i)
  }

  "testInterface_super_interfaces" should "nice" in {
    val statements = parse("interface A:B,C")
    assert(statements.size == 1)
    val s = statements.head

    val i = InterfaceStatement(
      "A",
      Set(),
      List(
        Access(null, "B", LineCol.SYNTHETIC),
        Access(null, "C", LineCol.SYNTHETIC)
      ),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == i)
  }


  "testInterface_super_interfaces_stmt" should "nice" in {
    val statements = parse("interface A:B,C\n    method()=...")
    assert(statements.size == 1)
    val s = statements.head

    val i = InterfaceStatement(
      "A",
      Set(),
      List(
        Access(null, "B", LineCol.SYNTHETIC),
        Access(null, "C", LineCol.SYNTHETIC)
      ),
      Set(),
      List(
        MethodStatement(
          "method",
          Set(),
          null,
          List(),
          Set(),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == i)
  }

  "testTryAll" should "nice" in {
    val statements = parse(""
      + "try\n"
      + "    a=1\n"
      + "catch e\n"
      + "    Exception,Throwable\n"
      + "        a=2\n"
      + "    RuntimeException\n"
      + "        a=3\n"
      + "finally\n"
      + "    a=4")
    assert(statements.size == 1)
    val s = statements.head

    val v1 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("1", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v2 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("2", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v3 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("3", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v4 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("4", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)

    val t = Try(
      List(v1),
      List(
        Catch(
          List(
            Access(null, "Exception", LineCol.SYNTHETIC),
            Access(null, "Throwable", LineCol.SYNTHETIC)
          ),
          List(v2),
          LineCol.SYNTHETIC
        ),
        Catch(
          List(
            Access(null, "RuntimeException", LineCol.SYNTHETIC),
          ),
          List(v3),
          LineCol.SYNTHETIC
        )
      ),
      "e",
      List(v4),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testTryOneCatch" should "nice" in {
    val statements = parse(""
      + "try\n"
      + "    a=1\n"
      + "catch e\n"
      + "    RuntimeException\n"
      + "        a = 3"
    )
    assert(statements.size == 1)
    val s = statements.head

    val v1 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("1", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v2 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("3", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)

    val t = Try(
      List(v1),
      List(
        Catch(
          List(
            Access(null, "RuntimeException", LineCol.SYNTHETIC),
          ),
          List(v2),
          LineCol.SYNTHETIC
        )
      ),
      "e",
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }


  "testTryOneCatchNoProcess" should "nice" in {
    val statements = parse(""
      + "try\n"
      + "    a=1\n"
      + "catch e\n"
      + "    RuntimeException\n"
    )
    assert(statements.size == 1)
    val s = statements.head

    val v1 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("1", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)


    val t = Try(
      List(v1),
      List(
        Catch(
          List(
            Access(null, "RuntimeException", LineCol.SYNTHETIC),
          ),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      "e",
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }


  "testTryTwoCatchOneProcess" should "nice" in {
    val statements = parse(""
      + "try\n"
      + "    a=1\n"
      + "catch e\n"
      + "    Exception,Throwable\n"
      + "    RuntimeException\n"
      + "        a = 3"
    )
    assert(statements.size == 1)
    val s = statements.head

    val v1 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("1", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v2 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("3", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)

    val t = Try(
      List(v1),
      List(
        Catch(
          List(
            Access(null, "Exception", LineCol.SYNTHETIC),
            Access(null, "Throwable", LineCol.SYNTHETIC)
          ),
          List(),
          LineCol.SYNTHETIC
        ),
        Catch(
          List(
            Access(null, "RuntimeException", LineCol.SYNTHETIC),
          ),
          List(v2),
          LineCol.SYNTHETIC
        )
      ),
      "e",
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }


  "testTryFinally" should "nice" in {
    val statements = parse(""
      + "try\n"
      + "    a=1\n"
      + "finally\n"
      + "    a=4")
    assert(statements.size == 1)
    val s = statements.head

    val v1 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("1", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)
    val v4 = VariableDef("a",
      Set(),
      null,
      NumberLiteral("4", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC)

    val t = Try(
      List(v1),
      List(),
      "",
      List(v4),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }


  "testThrow" should "nice" in {
    val statements = parse("throw e")
    assert(statements.size == 1)
    val s = statements.head

    val t = Throw(
      Access(null, "e", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }


  "testAnnoVariable" should "nice" in {
    val statements = parse("@Anno(abc=1)\ni=2")
    assert(statements.size == 1)
    val s = statements.head

    val t = VariableDef("i",
      Set(),
      null,
      NumberLiteral("2", LineCol.SYNTHETIC),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(Assignment(
            Access(null, "abc", LineCol.SYNTHETIC),
            "=",
            NumberLiteral("1", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          )),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC)
    assert(s == t)
  }


  "testAnnoInterface" should "nice" in {
    val statements = parse("@Anno(abc=1)\ninterface I")
    assert(statements.size == 1)
    val s = statements.head

    val t = InterfaceStatement("I",
      Set(),
      List(),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(Assignment(
            Access(null, "abc", LineCol.SYNTHETIC),
            "=",
            NumberLiteral("1", LineCol.SYNTHETIC),
            LineCol.SYNTHETIC
          )),
          LineCol.SYNTHETIC
        )
      ),
      List(),
      LineCol.SYNTHETIC)
    assert(s == t)
  }


  "testAnnoClass" should "nice" in {
    val statements = parse("@Anno\nclass I")
    assert(statements.size == 1)
    val s = statements.head

    val t = ClassStatement("I",
      Set(),
      List(),
      null,
      List(),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      List(),
      LineCol.SYNTHETIC)
    assert(s == t)
  }

  "testAnnoMethod" should "nice" in {
    val statements = parse("@Anno\nmethod()=...")
    assert(statements.size == 1)
    val s = statements.head

    val t = MethodStatement("method",
      Set(),
      null,
      List(),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        )
      ),
      List(),
      LineCol.SYNTHETIC)
    assert(s == t)
  }

  "testArrayExp" should "nice" in {
    val statements = parse("[1,2]")
    assert(statements.size == 1)
    val s = statements.head

    val arr = ArrayExp(
      List(
        NumberLiteral("1", LineCol.SYNTHETIC),
        NumberLiteral("2", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(arr == s)
  }


  "testArrayExp0" should "nice" in {
    val statements = parse("[]")
    assert(statements.size == 1)
    val s = statements.head

    val arr = ArrayExp(
      List(),
      LineCol.SYNTHETIC
    )
    assert(arr == s)
  }

  "testIndexAccess1" should "nice" in {
    val statements = parse("array[1]")
    assert(statements.size == 1)
    val s = statements.head

    val arr = Index(
      Access(null, "array", LineCol.SYNTHETIC),
      List(NumberLiteral("1", LineCol.SYNTHETIC)),
      LineCol.SYNTHETIC
    )
    assert(arr == s)
  }

  "testIndexAccess12" should "nice" in {
    val statements = parse("array[1,2]")
    assert(statements.size == 1)
    val s = statements.head

    val arr = Index(
      Access(null, "array", LineCol.SYNTHETIC),
      List(
        NumberLiteral("1", LineCol.SYNTHETIC),
        NumberLiteral("2", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(arr == s)
  }

  "testIndexAccess0" should "nice" in {
    val statements = parse("array[]")
    assert(statements.size == 1)
    val s = statements.head

    val arr = Index(
      Access(null, "array", LineCol.SYNTHETIC),
      List(),
      LineCol.SYNTHETIC
    )
    assert(arr == s)
  }

  "testMap" should "nice" in {
    val statements = parse("{a:b}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) -> Access(null, "b", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testMap2" should "nice" in {
    val statements = parse("{a:b,c:d}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) -> Access(null, "b", LineCol.SYNTHETIC),
        Access(null, "c", LineCol.SYNTHETIC) -> Access(null, "d", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testMapInMap" should "nice" in {
    val statements = parse("{a:{b:c}}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) ->
          MapExp(
            Map[Expression, Expression](
              Access(null, "b", LineCol.SYNTHETIC) -> Access(null, "c", LineCol.SYNTHETIC),
            ), LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testMapAssign" should "nice" in {
    val statements = parse("map = {a:b}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) -> Access(null, "b", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )

    val ass = VariableDef(
      "map",
      Set(),
      null,
      m,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(s == ass)
  }


  "testMapInMapAssign" should "nice" in {
    val statements = parse("map = {a:{b:c}}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) ->
          MapExp(
            Map[Expression, Expression](
              Access(null, "b", LineCol.SYNTHETIC) -> Access(null, "c", LineCol.SYNTHETIC),
            ), LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )

    val ass = VariableDef(
      "map",
      Set(),
      null,
      m,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(s == ass)
  }

  "testMapInMapPretty" should "nice" in {
    val statements = parse(""
      + "{\n"
      + "    a:{\n"
      + "        b:c\n"
      + "    }\n"
      + "}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map[Expression, Expression](
        Access(null, "a", LineCol.SYNTHETIC) ->
          MapExp(
            Map[Expression, Expression](
              Access(null, "b", LineCol.SYNTHETIC) -> Access(null, "c", LineCol.SYNTHETIC),
            ), LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testPkgDeclare1" should "nice" in {
    val statements = parse("# jes")
    assert(statements.size == 1)
    val s = statements.head

    val p = PackageDeclare(
      PackageRef("jes", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testPkgDeclare2" should "nice" in {
    val statements = parse("# jes::lang::util")
    assert(statements.size == 1)
    val s = statements.head

    val p = PackageDeclare(
      PackageRef("jes::lang::util", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }

  "testImportPackageAll" should "nice" in {
    val statements = parse("#> jes::lang::_")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        PackageRef("jes::lang", LineCol.SYNTHETIC),
        null,
        importAll = true
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportClass" should "nice" in {
    val statements = parse("#> jes::lang::Cls")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          PackageRef("jes::lang", LineCol.SYNTHETIC),
          "Cls",
          LineCol.SYNTHETIC
        ),
        importAll = false
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportClassAll" should "nice" in {
    val statements = parse("#> jes::lang::Cls._")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          PackageRef("jes::lang", LineCol.SYNTHETIC),
          "Cls",
          LineCol.SYNTHETIC
        ),
        importAll = true
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportInnerClass" should "nice" in {
    val statements = parse("#> jes::lang::Cls.Inner")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          Access(
            PackageRef("jes::lang", LineCol.SYNTHETIC),
            "Cls",
            LineCol.SYNTHETIC
          ),
          "Inner",
          LineCol.SYNTHETIC
        ),
        importAll = false
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportInnerClassAll" should "nice" in {
    val statements = parse("#> jes::lang::Cls.Inner._")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          Access(
            PackageRef("jes::lang", LineCol.SYNTHETIC),
            "Cls",
            LineCol.SYNTHETIC
          ),
          "Inner",
          LineCol.SYNTHETIC
        ),
        importAll = true
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportClassAllNoPKG" should "nice" in {
    val statements = parse("#> Cls._")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          null,
          "Cls",
          LineCol.SYNTHETIC
        ),
        importAll = true
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportInnerClassAllNoPKG" should "nice" in {
    val statements = parse("#> Cls.Inner._")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          Access(
            null,
            "Cls",
            LineCol.SYNTHETIC
          ),
          "Inner",
          LineCol.SYNTHETIC
        ),
        importAll = true
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testImportInnerClassNoPKG" should "nice" in {
    val statements = parse("#> Cls.Inner")
    assert(statements.size == 1)
    val s = statements.head

    val p = Import(
      List(ImportDetail
      (
        null,
        Access(
          Access(
            null,
            "Cls",
            LineCol.SYNTHETIC
          ),
          "Inner",
          LineCol.SYNTHETIC
        ),
        importAll = false
      )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == p)
  }


  "testWhile" should "nice" in {
    val statements = parse("" +
      "while true\n" +
      "    1")
    assert(statements.size == 1)
    val s = statements.head

    val w = WhileStatement(
      BoolLiteral("true", LineCol.SYNTHETIC),
      List(NumberLiteral("1", LineCol.SYNTHETIC)),
      doWhile = false,
      LineCol.SYNTHETIC
    )

    assert(w == s)
  }

  "testDoWhile" should "nice" in {
    val statements = parse("" +
      "do\n" +
      "    1\n" +
      "while true")
    assert(statements.size == 1)
    val s = statements.head

    val w = WhileStatement(
      BoolLiteral("true", LineCol.SYNTHETIC),
      List(NumberLiteral("1", LineCol.SYNTHETIC)),
      doWhile = true,
      LineCol.SYNTHETIC
    )

    assert(w == s)
  }


  "testAnnoArray" should "nice" in {
    val statements = parse("" +
      "@Anno(a=[1,2])\n" +
      "i=2")
    assert(statements.size == 1)
    val s = statements.head

    val v = VariableDef(
      "i",
      Set(),
      null,
      NumberLiteral("2", LineCol.SYNTHETIC),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(
            Assignment(
              Access(null, "a", LineCol.SYNTHETIC),
              "=",
              ArrayExp(
                List(NumberLiteral("1", LineCol.SYNTHETIC),
                  NumberLiteral("2", LineCol.SYNTHETIC)),
                LineCol.SYNTHETIC
              ),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "testAnnoNoAssign" should "nice" in {
    val statements = parse("" +
      "@Anno([1,2])\n" +
      "i=2")
    assert(statements.size == 1)
    val s = statements.head

    val v = VariableDef(
      "i",
      Set(),
      null,
      NumberLiteral("2", LineCol.SYNTHETIC),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(
            Assignment(
              Access(null, "value", LineCol.SYNTHETIC),
              "=",
              ArrayExp(
                List(NumberLiteral("1", LineCol.SYNTHETIC),
                  NumberLiteral("2", LineCol.SYNTHETIC)),
                LineCol.SYNTHETIC
              ),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "testClosure" should "nice" in {
    val statements = parse("(<1)")
    assert(statements.size == 1)
    val s = statements.head
    val p = Procedure(
      List(Return(NumberLiteral("1", LineCol.SYNTHETIC), LineCol.SYNTHETIC))
      , LineCol.SYNTHETIC
    )
    assert(s == p)
  }

  "testClosureMultipleLine" should "nice" in {
    val statements = parse(
      "(\n" +
        "    i=1\n" +
        "    <i\n" +
        ")"
    )
    assert(statements.size == 1)
    val s = statements.head
    val p = Procedure(
      List(
        VariableDef(
          "i",
          Set(),
          null,
          NumberLiteral("1", LineCol.SYNTHETIC),
          Set(),
          LineCol.SYNTHETIC
        ),
        Return(Access(null, "i", LineCol.SYNTHETIC), LineCol.SYNTHETIC))
      , LineCol.SYNTHETIC
    )
    assert(s == p)
  }

  "testArrayType" should "nice" in {
    val statements = parse(
      "i:[]Type"
    )
    assert(statements.size == 1)
    val s = statements.head
    val v = VariableDef(
      "i",
      Set(),
      Access(
        Access(null, "Type", LineCol.SYNTHETIC),
        "[]",
        LineCol.SYNTHETIC
      ),
      null,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "test2ArrayType" should "nice" in {
    val statements = parse(
      "i:[][]Type"
    )
    assert(statements.size == 1)
    val s = statements.head
    val v = VariableDef(
      "i",
      Set(),
      Access(
        Access(
          Access(null, "Type", LineCol.SYNTHETIC),
          "[]",
          LineCol.SYNTHETIC
        ),
        "[]",
        LineCol.SYNTHETIC),
      null,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "testTypeOf" should "nice" in {
    val statements = parse(
      "type int"
    )
    assert(statements.size == 1)
    val s = statements.head

    val t = TypeOf(
      Access(null, "int", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testIn" should "nice" in {
    val statements = parse(
      "1 in [1,2]"
    )
    assert(statements.size == 1)
    val s = statements.head

    val t = TwoVariableOperation(
      "in",
      NumberLiteral("1", LineCol.SYNTHETIC),
      ArrayExp(
        List(
          NumberLiteral("1", LineCol.SYNTHETIC),
          NumberLiteral("2", LineCol.SYNTHETIC)
        ),
        LineCol.SYNTHETIC
      ),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testDataClass" should "nice" in {
    val statements = parse(
      "data class Data(id,name)"
    )
    assert(statements.size == 1)
    val s = statements.head
    val c = ClassStatement(
      "Data",
      Set(
        Modifier(
          "data",
          LineCol.SYNTHETIC
        )
      ),
      List(
        VariableDef("id", Set(), null, null, Set(), LineCol.SYNTHETIC),
        VariableDef("name", Set(), null, null, Set(), LineCol.SYNTHETIC)
      ),
      null,
      List(),
      Set(),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s == c)
  }

  "testPass" should "nice" in {
    val statements = parse(
      "method()\n    ..."
    )
    assert(statements.size == 1)
    val s = statements.head

    val m = MethodStatement(
      "method",
      Set(),
      null,
      List(),
      Set(),
      List(
        Pass(LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testNull" should "nice" in {
    val statements = parse(
      "null"
    )
    assert(statements.size == 1)
    val s = statements.head

    assert(s == Null(LineCol.SYNTHETIC))
  }


  "testAsType" should "nice" in {
    val statements = parse(
      "1 as java::lang::List"
    )
    assert(statements.size == 1)
    val s = statements.head
    val a = AsType(
      NumberLiteral("1", LineCol.SYNTHETIC),
      Access(
        PackageRef("java::lang", LineCol.SYNTHETIC),
        "List",
        LineCol.SYNTHETIC
      ),
      LineCol.SYNTHETIC
    )
    assert(s == a)
  }


  "testUndefined" should "nice" in {
    val statements = parse(
      "undefined"
    )
    assert(statements.size == 1)
    val s = statements.head
    assert(s == UndefinedExp(LineCol.SYNTHETIC))
  }

  "testUnaryInc" should "nice" in {
    val statements = parse(
      "++i"
    )
    assert(statements.size == 1)
    val s = statements.head

    val u = UnaryOneVariableOperation(
      "++",
      Access(null, "i", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == u)
  }

  "testOperatorLikeInvocation1" should "nice" in {
    val statements = parse(
      "a op b\na op"
    )
    assert(statements.size == 2)
    val s = statements.head

    val i = Invocation(
      Access(
        Access(
          null, "a", LineCol.SYNTHETIC
        ),
        "op",
        LineCol.SYNTHETIC
      ),
      List(
        Access(
          null, "b", LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == i)

    val s1 = statements(1)
    val i1 = Invocation(
      Access(
        Access(
          null, "a", LineCol.SYNTHETIC
        ),
        "op",
        LineCol.SYNTHETIC
      ),
      List(),
      LineCol.SYNTHETIC
    )
    assert(s1 == i1)
  }


  "testOperatorLikeInvocation2" should "nice" in {
    val statements = parse(
      "db select a, b, c from user"
    )
    assert(statements.size == 1)
    val s = statements.head

    val i = Invocation(
      Access(
        Invocation(
          Access(
            Access(null, "db", null)
            , "select", LineCol.SYNTHETIC
          ),
          List(
            Access(null, "a", null),
            Access(null, "b", null),
            Access(null, "c", null)
          ),
          null
        ),
        "from",
        null
      ),
      List(Access(null, "user", null)),
      null
    )
    assert(s == i)
  }




}

object ParserTest {
  def parse(stmt: String): List[Statement] = {
    val processor = new Lexer("test", new BufferedReader(new StringReader(stmt)), 4)
    val root = processor.parse
    val syntacticProcessor = Parser(root)
    syntacticProcessor.parse
  }
}
