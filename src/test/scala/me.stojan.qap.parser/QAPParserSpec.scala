package me.stojan.qap.parser

import me.stojan.qap.QAPSpec

import shapeless._

import scala.util.{ Success, Failure }


class QAPParserSpec extends QAPSpec {

  "QAPParser" should "parse out assignment names" in {
    def valid(name: String): Unit = (new QAPParser(name).AssignmentName.run()) should be (Success(name))
    def invalid(name: String): Unit = (new QAPParser(name).AssignmentName.run()) should be a 'failure

    valid("_")
    valid("a")
    valid("b")
    valid("A")
    valid("aB")
    valid("a_")
    valid("aB_cD")
    valid("a123")
    valid("a123_")
    valid("___")
    valid("aBc12_34dXyZ")

    invalid("1")
    invalid("!")
    invalid("#2")
  }

  it should "parse out mandatory whitespace" in {
    def valid(str: String): Unit = (new QAPParser(str).WSM.run()) should be (Success())
    def invalid(str: String): Unit = (new QAPParser(str).WSM.run()) should be a 'failure

    valid(" ")
    valid("  ")
    valid("   ")
    valid(" \t \t\t")
    valid("\t")
    valid("\n")
    valid("\r")
    valid("\r\n")
    valid("\n\r")
    valid("\n\r\t \n\n\r\n  \t \t\n\r\n")

    invalid("a")
    invalid("b")
    invalid("#")
    invalid("3")
    invalid("$")
  }

  it should "parse out whitespace" in {
    def valid(str: String): Unit = (new QAPParser(str).WS.run()) should be (Success())
    def invalid(str: String): Unit = (new QAPParser(str).WS.run()) should be a 'failure

    valid(" ")
    valid("  ")
    valid("   ")
    valid(" \t \t\t")
    valid("\t")
    valid("\n")
    valid("\r")
    valid("\r\n")
    valid("\n\r")
    valid("\n\r\t \n\n\r\n  \t \t\n\r\n")

    valid("abc")
  }

  it should "define space properly" in {
    def check(str: String): Unit = (new QAPParser(str).Space.run()) should be (Success())

    check(" ")
    check("\t")
    check("\r")
    check("\n")
  }

  it should "define an input assignment" in {
    def check(name: String, extra: String): Unit = (new QAPParser(name + extra).AssignInput.run()) should be (Success(InputAssignment(name)))

    check("a", "=input")
    check("ab", " =input")
    check("_", " = input")
    check("_12", "\n=input")
    check("XyZ", "\n=\ninput")
  }

  it should "define an assignment list properly" in {
    def check(sep: String, assignments: Seq[String], pref: String = "", suff: String = ""): Unit = 
      (new QAPParser("(" + pref + assignments.mkString(sep) + suff + ")").AssignmentList.run()) should be (Success(assignments))

    def invalid(str: String): Unit = (new QAPParser(str).AssignmentList.run()) should be a 'failure

    val assignments = Seq("a", "_", "_1", "xA")

    check(",", assignments)
    check(", ", assignments, " ", "\t")
    check(" ,", assignments)
    check(" , ", assignments, "\n\r\n\n\t", "\t\n\r")
    check("\n,\r", assignments)
    check("\n\r,\t", assignments)
    check("---", Seq("a"))

    invalid("(a, b")
    invalid("(a, b,, c)")
    invalid("x, y, z)")
    invalid("(31, y, z)")
    invalid("()")
    invalid("a, b, c")
  }

  it should "capture hex values correctly" in {
    def check(v: BigInt): Unit =
      (new QAPParser("0x" + v.toString(16)).HexValue.run()) should be (Success(v))

    def invalid(s: String): Unit =
      (new QAPParser(s).HexValue.run()) should be a 'failure

    check(13)
    check(15)
    check(328923132)
    check(BigInt("8120349812904812904274812237498248"))
    check(0)

    invalid("01232")
    invalid("0s23981")
    invalid(" ")
    invalid("abe")
    invalid("ff")
  }

  it should "capture base-10 values correctly" in {
    def check(v: BigInt): Unit =
      (new QAPParser(v.toString(10)).TenValue.run()) should be (Success(v))

    def invalid(s: String): Unit =
      (new QAPParser(s).TenValue.run()) should be a 'failure

    check(123)
    check(BigInt("2318039812"))
    check(3)
    check(0)

    invalid("a23")
  }

  it should "define a constant assignment properly" in {
    def valid(n: String, v: BigInt, s: String): Unit =
      (new QAPParser(s).AssignConstant.run()) should be (Success(ConstantAssignment(n, v)))

    valid("a", 16, "a=16")
    valid("b", 16, "b=0x10")
    valid("_", 18, "_ = 18")
    valid("__", 18, "__\n\r=\t018")
  }

  it should "define name-on-left multiplication correctly" in {
    def valid(n: String, v: BigInt, s: String): Unit =
      (new QAPParser(s).MLeft.run()) should be (Success(AssignmentCoefficient(n, v)))

    def invalid(s: String): Unit =
      (new QAPParser(s).MLeft.run()) should be a 'failure

    valid("a", 10, "a * 10")
    valid("b", 2, "b*2")
    valid("x", 0, "x * 0")

    invalid("3 * x")
    invalid("x**3")
    invalid("x")
    invalid("x *")
    invalid("3")
  }

  it should "define name-on-right multiplication correctly" in {
    def valid(n: String, v: BigInt, s: String): Unit =
      (new QAPParser(s).MRight.run()) should be (Success(AssignmentCoefficient(n, v)))

    def invalid(s: String): Unit =
      (new QAPParser(s).MRight.run()) should be a 'failure

    valid("a", 3, "3 * a")
    valid("_", 1, "1*_")
    valid("abcd", 0, "0* abcd")
    valid("ss", 16, "0x10*ss")

    invalid("3")
    invalid("a")
    invalid("a**3")
    invalid("*3")
    invalid("a*")
    invalid("a*.b")
  }

  it should "define value-only multiplication correctly" in {
    def valid(v: BigInt, s: String): Unit =
      (new QAPParser(s).MValue.run()) should be (Success(ConstantCoefficient(v)))

    valid(3, "3")
    valid(16, "0x10")
  }

  it should "define name-only multiplication correctly" in {
    def valid(n: String, s: String): Unit =
      (new QAPParser(s).MName.run()) should be (Success(AssignmentCoefficient(n, 1)))

    valid("a", "a")
    valid("b", "b")
    valid("c", "c")
    valid("__", "__")
  }

  it should "differentiate between named multiplications" in {
    def valid(n: String, v: BigInt, s: String): Unit =
      (new QAPParser(s).Multiplication.run()) should be (Success(AssignmentCoefficient(n, v)))

    valid("a", 3, "a * 3")
    valid("_", 1, "_")
    valid("x", 8, "8 * x")
    valid("s", 16, "0x10*s")
  }

  it should "differentiate between name-on-right and value multiplication" in {
    def validN(n: String, v: BigInt, s: String): Unit =
      (new QAPParser(s).Multiplication.run()) should be (Success(AssignmentCoefficient(n, v)))

    def validC(v: BigInt, s: String): Unit =
      (new QAPParser(s).Multiplication.run()) should be (Success(ConstantCoefficient(v)))

    validN("x", 3, "3 * x")
    validC(3, "3")
  }

  it should "define addititon correctly" in {
    def valid(s: String, v: Seq[Coefficient]): Unit = 
      (new QAPParser(s).Addition.run()) should be (Success(v))

    valid("1 * a + b * 2 + c", Seq(
      AssignmentCoefficient("a", 1), 
      AssignmentCoefficient("b", 2), 
      AssignmentCoefficient("c", 1)))

    valid("2 + b * 1", Seq(
      ConstantCoefficient(2),
      AssignmentCoefficient("b", 1)))
  }

  it should "define gate value correctly" in {
    def valid(s: String, v: (Seq[Coefficient], Seq[Coefficient])): Unit =
      (new QAPParser(s).GateValue.run()) should be (Success(v._1 :: v._2 :: HNil))

    valid("(x + 3*y + 0) * (0)", (
      Seq(
        AssignmentCoefficient("x", 1),
        AssignmentCoefficient("y", 3),
        ConstantCoefficient(0)),
      Seq(
        ConstantCoefficient(0))))
  }

  it should "define gates correctly" in {
    def valid(s: String, n: String, i: Seq[String], left: Seq[Coefficient], right: Seq[Coefficient]): Unit =
      (new QAPParser(s).GateDefinition.run()) should be (Success(GateStatement(n, i, left, right)))

    valid("gate name(a, b): (a * 1 + b * 2) * (1)", 
      "name",
      Seq("a", "b"),
      Seq(AssignmentCoefficient("a", 1), AssignmentCoefficient("b", 2)),
      Seq(ConstantCoefficient(1)))
  }

  it should "define gate assignment correctly" in {
    def valid(s: String, n: String, g: String, a: Seq[String]): Unit =
      (new QAPParser(s).AssignGate.run()) should be (Success(GateAssignment(n, g, a)))

    def invalid(s: String): Unit =
      (new QAPParser(s).AssignGate.run()) should be a 'failure

    valid("x = gate(a, b)", "x", "gate", Seq("a", "b"))
    valid("x=m(y)", "x", "m", Seq("y"))

    invalid("x = y()")
    invalid("x = y")
    invalid("x = input")
    invalid("a = 3")
  }

  it should "define statements correctly" in {
    def valid(s: String, statements: Seq[Statement]): Unit =
      (new QAPParser(s).Statements.run()) should be (Success(statements))

    def invalid(s: String): Unit = 
      (new QAPParser(s).Statements.run()) should be a 'failure

    valid("a = input; b = input; c = n(a, b);", Seq(
      InputAssignment("a"),
      InputAssignment("b"),
      GateAssignment("c", "n", Seq("a", "b"))))

    valid("a = input\n\n\r\n;  \n\r  ", Seq(InputAssignment("a")))

    valid(" \n \na = input\n\n\r\n;  \n\r  ", Seq(InputAssignment("a")))

    invalid("a = input")
    invalid("a =")

    valid(" \n\r\t", Seq())
  }

}

