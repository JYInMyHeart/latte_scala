package latte

import java.io.{BufferedReader, StringReader}

class LexerTest extends UnitSpec {
  "# lt.test" should "be a new startNode" in {
    val processor = new Lexer("test", new BufferedReader(new StringReader("# lt::test")), 4)
    val root = processor.parse

    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("#", args)
    root2.linkNode = args.previous
    val startNode = ElementStartNode(args, 4)
    args.previous = startNode
    args.startNodeStack.push(startNode)
    args.previous = null
    args.previous = Element("it", args)
    args.previous = Element("::", args)
    args.previous = Element("test", args)
    assert(root2 == root)
  }

  "#> packageName._" should "be a new startNode" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("" + "#> package::name::*\n" + "    package::name::Test")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("#>", args)
    root2.linkNode = args.previous
    val startNode = ElementStartNode(args, 4)
    args.previous = null
    args.previous = Element("package", args)
    startNode.linkNode = args.previous
    args.previous = Element("::", args)
    args.previous = Element("name", args)
    args.previous = Element("::", args)
    args.previous = Element("*", args)
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element("package", args)
    args.previous = Element("::", args)
    args.previous = Element("name", args)
    args.previous = Element("::", args)
    Element("Test", args)
    assert(root2 == root)
  }

  "class ClassName" should "be a class node" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("class ClassName")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("class", args)
    root2.linkNode = args.previous
    Element("ClassName", args)
    assert(root2 == root)
  }


  "class ClassName(arg1:Type1,arg2:Type2)" should "be a class node" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("class ClassName(arg1,arg2=value2)")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("class", args)
    root2.linkNode = args.previous
    args.previous = Element("ClassName", args)
    args.previous = Element("(", args)
    val startNode = ElementStartNode(args, 4)
    args.previous = startNode
    args.previous = EndingNode(EndingNode.WEAK, args)
    Element(")", args)
    args.previous = null
    args.previous = Element("arg1", args)
    startNode.linkNode = args.previous
    args.previous = EndingNode(EndingNode.STRONG, args)
    args.previous = Element("arg2", args)
    args.previous = Element("=", args)
    args.previous = Element("value2", args)
    assert(root2 == root)
  }

  "variable" should "be a value node" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("val value = 1")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("val", args)
    root2.linkNode = args.previous
    args.previous = Element("value", args)
    args.previous = Element("=", args)
    Element("1", args)
    assert(root2 == root)
  }

  "method" should "be a method node" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("" + "val trim(input)\n" + "    <input.trim()")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("val", args)
    root2.linkNode = args.previous
    args.previous = Element("trim", args)
    args.previous = Element("(", args)
    val startNode = ElementStartNode(args, 4)
    args.previous = startNode
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element(")", args)
    val startNode1 = ElementStartNode(args, 4)
    args.previous = null
    args.previous = Element("input", args)
    startNode.linkNode = args.previous
    args.previous = null
    args.previous = Element("<", args)
    startNode1.linkNode = args.previous
    args.previous = Element("input", args)
    args.previous = Element(".", args)
    args.previous = Element("trim", args)
    args.previous = Element("(", args)
    args.previous = Element(")", args)
    assert(root == root2)
  }

  "method2" should "be a method node" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("" + "voidMethod(input)=0")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("voidMethod", args)
    root2.linkNode = args.previous
    args.previous = Element("(", args)
    val startNode = ElementStartNode(args, 4)
    args.previous = startNode
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element(")", args)
    args.previous = Element("=", args)
    args.previous = Element("0", args)
    args.previous = null
    args.previous = Element("input", args)
    startNode.linkNode = args.previous
    assert(root == root2)
  }

  "modifiers" should "nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("" + "pub val abs class X")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("pub", args)
    root2.linkNode = args.previous
    args.previous = Element("val", args)
    args.previous = Element("abs", args)
    args.previous = Element("class", args)
    args.previous = Element("X", args)
    assert(root == root2)
  }


  "if" should "nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader(""
          + "if true\n"
          + "    <\"hello world\"\n"
          + "elseif false\n"
          + "    <\"hello\"\n"
          + "else\n"
          + "    <\"world\"")), 4)
    val root = processor.parse
    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("if", args)
    root2.linkNode = args.previous
    args.previous = Element("true", args)
    val startNode1 = ElementStartNode(args, 4)
    args.previous = startNode1
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element("elseif", args)
    args.previous = Element("false", args)
    val startNode2 = ElementStartNode(args, 4)
    args.previous = startNode2
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element("else", args)
    val startNode3 = ElementStartNode(args, 4)
    args.previous = null
    args.previous = Element("<", args)
    startNode1.linkNode = args.previous
    args.previous = Element("\"hello world\"", args)
    args.previous = null
    args.previous = Element("<", args)
    startNode2.linkNode = args.previous
    args.previous = Element("\"hello\"", args)
    args.previous = null
    args.previous = Element("<", args)
    startNode3.linkNode = args.previous
    args.previous = Element("\"world\"", args)
    assert(root == root2)
  }

  "lambda" should "be nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader("list.stream().filter(\n    (e)->e>10)")), 4)
    val root = processor.parse

    val args = Args()
    val root2 = ElementStartNode(args, 0)
    args.previous = Element("list", args)
    root2.linkNode = args.previous
    args.previous = Element(".", args)
    args.previous = Element("stream", args)
    args.previous = Element("(", args)
    args.previous = Element(")", args)
    args.previous = Element(".", args)
    args.previous = Element("filter", args)
    args.previous = Element("(", args)
    val startNode1 = ElementStartNode(args, 4)
    args.previous = startNode1
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element(")", args)

    args.previous = null
    args.previous = Element("(", args)
    startNode1.linkNode = args.previous
    val startNode2 = ElementStartNode(args, 8)
    args.previous = startNode2
    args.previous = EndingNode(EndingNode.WEAK, args)
    args.previous = Element(")", args)
    args.previous = Element("->", args)
    val startNode3 = ElementStartNode(args, 8)

    args.previous = null
    args.previous = Element("e", args)
    startNode2.linkNode = args.previous

    args.previous = null
    args.previous = Element("e", args)
    startNode3.linkNode = args.previous
    args.previous = Element(">", args)
    args.previous = Element("10", args)

    assert(root2 == root)
  }

  "operators" should "nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader(""
          + "1+2\n"
          + "3-4\n"
          + "5*6\n"
          + "7/8\n"
          + "9%10\n"
          + "i+=11\n"
          + "i-=12\n"
          + "i*=13\n"
          + "i/=14\n"
          + "i%=15\n"
          + "i++\n"
          + "i--\n"
          + "++i\n"
          + "--i\n"
          + "x=:=y\n"
          + "x!:=y")), 4)
    val root = processor.parse

    val args = Args()
    val root2 = ElementStartNode(args, 0)

    args.previous = Element("1", args)
    root2.linkNode = args.previous
    args.previous = Element("+", args)
    args.previous = Element("2", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("3", args)
    args.previous = Element("-", args)
    args.previous = Element("4", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("5", args)
    args.previous = Element("*", args)
    args.previous = Element("6", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("7", args)
    args.previous = Element("/", args)
    args.previous = Element("8", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("9", args)
    args.previous = Element("%", args)
    args.previous = Element("10", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("+=", args)
    args.previous = Element("11", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("-=", args)
    args.previous = Element("12", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("*=", args)
    args.previous = Element("13", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("/=", args)
    args.previous = Element("14", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("%=", args)
    args.previous = Element("15", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("++", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("i", args)
    args.previous = Element("--", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("++", args)
    args.previous = Element("i", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("--", args)
    args.previous = Element("i", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("x", args)
    args.previous = Element("=:=", args)
    args.previous = Element("y", args)

    args.previous = EndingNode(EndingNode.WEAK, args)

    args.previous = Element("x", args)
    args.previous = Element("!:=", args)
    args.previous = Element("y", args)

    assert(root2 == root)
  }


  "indent" should "be 2" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader(""
          + "if true\n"
          + "  <\"hello world\"\n"
          + "elseif false\n"
          + "  <\"hello\"\n"
          + "else\n"
          + "  <\"world\"")), 2)
    val root = processor.parse
    assert(root != null)
  }


  /*
  *  step1 create startNode  args   then startNode.args = args   args.stack.push(node)
  *  step2 parse line   define a as b   detect as  and string
  *  then  replace a from rest string to b then parse rest string
  *  step3 split source line by some key words
  */
  "define" should "be nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader(""
          + "define 'CREATE TABLE' as 'class'\n"
          + "CREATE TABLE User")), 2)
    val root = processor.parse
    var node = root.linkNode
    assertResult(true)(node.isInstanceOf[Element])
    assert("class" == node.asInstanceOf[Element].content)
    node = node.next
    assertResult(true)(node.isInstanceOf[Element])
    assert("User" == node.asInstanceOf[Element].content)
    assert(node.next == null)
  }

  "undef" should "ne nice" in {
    val processor = new Lexer("test",
      new BufferedReader(
        new StringReader(""
          + "define 'A' as 'class'\n"
          + "undef 'A'")), 2)
    val root = processor.parse
  }


}
