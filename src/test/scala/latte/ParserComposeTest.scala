package latte

import java.io.{BufferedReader, StringReader}

import latte.ParserComposeTest.parse

import scala.collection.mutable.ListBuffer

class ParserComposeTest extends UnitSpec {
  "test2VarOperatorAndAccessAndPar" should "be nice" in {
    val statements = parse(
      "list * (1+2)"
    )
    assert(statements.size == 1)
    val s = statements.head
    val tvo = TwoVariableOperation(
      "*",
      Access(null, "list", LineCol.SYNTHETIC),
      TwoVariableOperation(
        "+",
        NumberLiteral("1", LineCol.SYNTHETIC),
        NumberLiteral("2", LineCol.SYNTHETIC),
        LineCol.SYNTHETIC
      ),
      LineCol.SYNTHETIC
    )
    assert(s == tvo)
  }

  "testAnnotationOnWrongPosition" should "be bad" in {
    try {
      val statements = parse(
        "" + "@Anno()\n" + "1+1"
      )
    } catch {
      case _: SyntaxException =>
    }
  }

  "testModifierOnWrongPosition" should "be bad" in {
    try {
      parse("pri 1")
    } catch {
      case _: SyntaxException =>
    }
  }

  "testAccessDotAndOperator1" should "be nice" in {
    val statements = parse(
      "1+a.b.c+2"
    )
    assert(statements.size == 1)
    val s = statements.head
    val tvo = TwoVariableOperation(
      "+",
      TwoVariableOperation(
        "+",
        NumberLiteral("1", LineCol.SYNTHETIC),
        Access(
          Access(
            Access(null, "a", LineCol.SYNTHETIC),
            "b",
            LineCol.SYNTHETIC
          ),
          "c",
          LineCol.SYNTHETIC
        ),
        LineCol.SYNTHETIC
      ),
      NumberLiteral("2", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == tvo)
  }

  "testAccessDotAndOperator2" should "be nice" in {
    val statements = parse(
      "1+list.get(0)+2"
    )
    assert(statements.size == 1)
    val s = statements.head

    val tvo = TwoVariableOperation(
      "+",
      TwoVariableOperation(
        "+",
        NumberLiteral("1", LineCol.SYNTHETIC),
        Invocation(
          Access(
            Access(
              null,
              "list",
              LineCol.SYNTHETIC
            ),
            "get",
            LineCol.SYNTHETIC
          ),
          List(NumberLiteral("0", LineCol.SYNTHETIC)),
          LineCol.SYNTHETIC
        ),
        LineCol.SYNTHETIC
      ),
      NumberLiteral("2", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == tvo)
  }

  "testClosureAndOperator" should "be nice" in {
    val statements = parse(
      "1+(\n" +
        "    if i==1\n" +
        "        <5\n" +
        "    <2\n" +
        ")+2")

    assert(statements.size == 1)
    val s = statements.head

    val tvo = TwoVariableOperation(
      "+",
      TwoVariableOperation(
        "+",
        NumberLiteral("1", LineCol.SYNTHETIC),
        Procedure(
          List(
            IfStatement(
              List(IfPair(
                TwoVariableOperation(
                  "==",
                  Access(null, "i", LineCol.SYNTHETIC),
                  NumberLiteral("1", LineCol.SYNTHETIC),
                  LineCol.SYNTHETIC
                ),
                List(
                  Return(
                    NumberLiteral("5", LineCol.SYNTHETIC),
                    LineCol.SYNTHETIC
                  )
                ),
                LineCol.SYNTHETIC
              )),
              LineCol.SYNTHETIC
            ),
            Return(
              NumberLiteral("2", LineCol.SYNTHETIC),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        ),
        LineCol.SYNTHETIC
      ),
      NumberLiteral("2", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(tvo == s)
  }

  "testMethodMultipleAnnotation" should "be nice" in {
    val statements = parse(
      "@Anno\n" +
        "pub method(\n" +
        "    @Anno1\n" +
        "    arg0\n" +
        "    @Anno2\n" +
        "    arg1" +
        "):Unit")
    assert(statements.size == 1)
    val s = statements.head

    val m = MethodStatement(
      "method",
      Set(
        Modifier(
          "pub",
          LineCol.SYNTHETIC
        )
      ),
      Access(null, "Unit", LineCol.SYNTHETIC),
      ListBuffer(
        VariableDef(
          "arg0",
          Set(),
          null,
          null,
          Set(
            Anno(
              Access(null, "Anno1", LineCol.SYNTHETIC),
              List(),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        ),
        VariableDef(
          "arg1",
          Set(),
          null,
          null,
          Set(
            Anno(
              Access(null, "Anno2", LineCol.SYNTHETIC),
              List(),
              LineCol.SYNTHETIC
            )
          ),
          LineCol.SYNTHETIC
        )
      ),
      Set(
        Anno(
          Access(null, "Anno", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        )),
      ListBuffer(),
      LineCol.SYNTHETIC
    )
    assert(s == m)

  }

  "testMethodDefInitVal" should "be bad" in {
    try {
      parse("method(arg0,arg1=1,arg2)=0")
    } catch {
      case _: SyntaxException =>
    }
  }

  "testMethodDefInitValPass" should "be bad" in {
    try {
      parse("method(arg0,arg1=1,arg2=2)=0")
    } catch {
      case _: SyntaxException =>
    }
  }

  "testClassDefInitValPass" should "be bad" in {
    try {
      parse("class C(arg0,arg1=1,arg2=2)")
    } catch {
      case _: SyntaxException => fail()
    }
  }

  "testReturnLittlerThan" should "be nice" in {
    val statements = parse("<3<2")
    assert(statements.size == 1)
    val s = statements.head

    val r = Return(
      TwoVariableOperation(
        "<",
        NumberLiteral("3", LineCol.SYNTHETIC),
        NumberLiteral("2", LineCol.SYNTHETIC),
        LineCol.SYNTHETIC
      ),
      LineCol.SYNTHETIC
    )
    assert(s == r)
  }

  "testMultipleLines" should "be nice" in {
    val statements = parse("" + "3+\n" + "2")
    assert(statements.size == 1)
    val s = statements.head

    val r = TwoVariableOperation(
      "+",
      NumberLiteral("3", LineCol.SYNTHETIC),
      NumberLiteral("2", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == r)
  }

  "testPrimitive" should "be nice" in {
    val statements = parse("i:int")
    assert(statements.size == 1)
    val s = statements.head
    val v = VariableDef(
      "i",
      Set(),
      Access(null, "int", LineCol.SYNTHETIC),
      null,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(s == v)
  }

  "testPrimitiveAssign" should "be nice" in {
    val statements = parse("i:bool=true")
    assert(statements.size == 1)
    val s = statements.head
    val v = VariableDef(
      "i",
      Set(),
      Access(null, "bool", LineCol.SYNTHETIC),
      BoolLiteral("true", LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC
    )
    assert(s == v)
  }

  "testMethodPrimitive" should "be nice" in {
    val statements = parse("method():bool")
    assert(statements.size == 1)
    val s = statements.head
    val v = MethodStatement(
      "method",
      Set(),
      Access(null, "bool", LineCol.SYNTHETIC),
      ListBuffer(),
      Set(),
      ListBuffer(),
      LineCol.SYNTHETIC
    )
    assert(s == v)
  }

  "testMapList" should "be nice" in {
    val statements = parse("{a:[1,2]}")
    assert(statements.size == 1)
    val s = statements.head
    val v = MapExp(
      Map(
        Access(null, "a", LineCol.SYNTHETIC) ->
          ArrayExp(
            List(
              NumberLiteral("1", LineCol.SYNTHETIC),
              NumberLiteral("2", LineCol.SYNTHETIC)
            ),
            LineCol.SYNTHETIC
          )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == v)
  }

  "testListMap" should "be nice" in {
    val statements = parse("[{'a':b},{'c':d}]")
    assert(statements.size == 1)
    val s = statements.head
    val m1 = MapExp(
      Map(
        StringLiteral("'a'", LineCol.SYNTHETIC) ->
          Access(null, "b", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    val m2 = MapExp(
      Map(
        StringLiteral("'c'", LineCol.SYNTHETIC) ->
          Access(null, "d", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    val v = ArrayExp(
      List(
        m1,
        m2
      ),
      LineCol.SYNTHETIC
    )
    assert(s == v)
  }

  "testMapWithListAndOther" should "be nice" in {
    val statements = parse("{a:[1,2],b:c}")
    assert(statements.size == 1)
    val s = statements.head

    val m = MapExp(
      Map(
        Access(null, "a", LineCol.SYNTHETIC) ->
          ArrayExp(
            List(
              NumberLiteral("1", LineCol.SYNTHETIC),
              NumberLiteral("2", LineCol.SYNTHETIC)
            ),
            LineCol.SYNTHETIC
          ),
        Access(null, "b", LineCol.SYNTHETIC) ->
          Access(null, "c", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(m == s)
  }

  "testListWithMapAndOther" should "be nice" in {
    val statements = parse("[a,{'c':d}]")
    assert(statements.size == 1)
    val s = statements.head

    val a = ArrayExp(
      List(
        Access(null, "a", LineCol.SYNTHETIC),
        MapExp(
          Map(
            StringLiteral("'c'", LineCol.SYNTHETIC) ->
              Access(null, "d", LineCol.SYNTHETIC)
          ),
          LineCol.SYNTHETIC
        )
      ),
      LineCol.SYNTHETIC
    )
    assert(s == a)
  }

  "testArrayPrimitive" should "be nice" in {
    val statements = parse("i:[]int")
    assert(statements.size == 1)
    val s = statements.head

    val v = VariableDef(
      "i",
      Set(),
      Access(Access(null, "int", LineCol.SYNTHETIC), "[]", LineCol.SYNTHETIC),
      null,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "testArrayPkgClass" should "nice" in {
    val statements = parse("i:[]java::util::List")
    assert(statements.size == 1)
    val s = statements.head

    val v = VariableDef(
      "i",
      Set(),
      Access(
        Access(PackageRef("java::util", LineCol.SYNTHETIC), "List", LineCol.SYNTHETIC),
        "[]",
        LineCol.SYNTHETIC),
      null,
      Set(),
      LineCol.SYNTHETIC
    )
    assert(v == s)
  }

  "testTypeOfArray" should "be nice" in {
    val statements = parse("type []int")
    assert(statements.size == 1)
    val s = statements.head

    val t = TypeOf(
      Access(
        Access(null, "int", LineCol.SYNTHETIC),
        "[]",
        LineCol.SYNTHETIC
      ),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testIndexAssign" should "be nice" in {
    val statements = parse("a[1]=2")
    assert(statements.size == 1)
    val s = statements.head

    val t = Assignment(
      Access(
        Index(
          Access(
            null,
            "a",
            LineCol.SYNTHETIC
          ),
          List(NumberLiteral("1", LineCol.SYNTHETIC)),
          LineCol.SYNTHETIC
        ),
        null,
        LineCol.SYNTHETIC
      ),
      "=",
      NumberLiteral("2", LineCol.SYNTHETIC),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testUndefinedAssign" should "be nice" in {
    val statements = parse("a=undefined")
    assert(statements.size == 1)
    val s = statements.head

    val t = VariableDef(
      "a",
      Set(),
      null,
      UndefinedExp(LineCol.SYNTHETIC),
      Set(),
      LineCol.SYNTHETIC
    )
    assert(s == t)
  }

  "testStaticStatement" should "be nice" in {
    val statements = parse(
      ""
        + "static method():Unit\n"
        + "static field=1\n"
        + "static System.out.println('hello world')"
    )
    assert(statements.size == 3)
    val s1 = statements.head
    val s2 = statements(1)
    val s3 = statements(2)
    assert(s1.isInstanceOf[StaticScope])
    assert(s2.isInstanceOf[StaticScope])
    assert(s3.isInstanceOf[StaticScope])
    assert(s1.asInstanceOf[StaticScope].statements.head.isInstanceOf[MethodStatement])
    assert(s2.asInstanceOf[StaticScope].statements.head.isInstanceOf[VariableDef])
    assert(s3.asInstanceOf[StaticScope].statements.head.isInstanceOf[Invocation])
  }

  "testMap_OperatorLikeInvocation" should "be nice" in {
    val statements = parse(
      "" +
        "{\n" +
        "    'a':a op b\n" +
        "    a op b:'b'\n" +
        "    'a':a op\n" +
        "    a op:'b'\n" +
        "}")

    assert(statements.size == 1)
    val s = statements.head
    val m = MapExp(
      Map(
        StringLiteral("'a'", LineCol.SYNTHETIC) ->
          Invocation(
            Access(Access(null, "a", LineCol.SYNTHETIC), "op", LineCol.SYNTHETIC),
            List(Access(null, "b", LineCol.SYNTHETIC)),
            LineCol.SYNTHETIC
          ),
        Invocation(
          Access(Access(null, "a", LineCol.SYNTHETIC), "op", LineCol.SYNTHETIC),
          List(Access(null, "b", LineCol.SYNTHETIC)),
          LineCol.SYNTHETIC
        ) ->
          StringLiteral("'b'", LineCol.SYNTHETIC),
        StringLiteral("'a'", LineCol.SYNTHETIC) ->
          Invocation(
            Access(Access(null, "a", LineCol.SYNTHETIC), "op", LineCol.SYNTHETIC),
            List(),
            LineCol.SYNTHETIC),
        Invocation(
          Access(Access(null, "a", LineCol.SYNTHETIC), "op", LineCol.SYNTHETIC),
          List(),
          LineCol.SYNTHETIC
        ) ->
          StringLiteral("'b'", LineCol.SYNTHETIC)
      ),
      LineCol.SYNTHETIC
    )
    assert(s == m)
  }

  "testOperatorLikeInvocationWithTwoVarOp" should "be nice" in {
    val statements = parse("a op b + 1 op 2 op")

    assert(statements.size == 1)
    val s = statements.head
    val i = Invocation(
      Access(
        Invocation(
          Access(
            Invocation(
              Access(
                Access(null, "a", null),
                "op",
                null
              ),
              List(
                TwoVariableOperation(
                  "+",
                  Access(
                    null,
                    "b",
                    null
                  ),
                  NumberLiteral("1", null),
                  null
                )
              ),
              null
            ),
            "op",
            null
          ),
          List(NumberLiteral("2", null)),
          null
        ),
        "op",
        null
      ),
      List(),
      null
    )
    assert(i == s)
  }

}

object ParserComposeTest {
  def parse(stmt: String): List[Statement] = {
    val processor = new Lexer("test", new BufferedReader(new StringReader(stmt)), 4)
    val root = processor.parse
    val syntacticProcessor = Parser(root)
    syntacticProcessor.parse
  }
}
