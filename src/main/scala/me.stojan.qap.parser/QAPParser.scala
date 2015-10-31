package me.stojan.qap.parser

import org.parboiled2._
import java.math.BigInteger

class QAPParser(val input: ParserInput) extends Parser {

  def Statements = rule {
    WS ~ zeroOrMore((Assign | GateDefinition) ~ WS ~ ';' ~ WS) ~ EOI
  }

  def Assign = rule {
    AssignConstant | AssignInput | AssignGate
  }

  def AssignInput = rule {
    (AssignmentName ~ WS ~ EqualsT ~ WS ~ InputT) ~> InputAssignment
  }

  def AssignConstant = rule {
    (AssignmentName ~ WS ~ EqualsT ~ WS ~ Value) ~> ConstantAssignment
  }

  def AssignGate = rule {
    (AssignmentName ~ WS ~ EqualsT ~ WS ~ AssignmentName ~ WS ~ AssignmentList) ~> GateAssignment
  }

  def GateDefinition = rule {
    (GateT ~ WSM ~ AssignmentName ~ WS ~ AssignmentList ~ WS ~ ':' ~ WS ~ GateValue) ~> GateStatement
  }

  def GateValue = rule {
    GateSide ~ WS ~ '*' ~ WS ~ GateSide
  }

  def GateSide = rule {
    '(' ~ WS ~ Addition ~ WS ~ ')'
  }

  def Addition = rule {
    Multiplication * (WS ~ '+' ~ WS)
  }

  def Multiplication = rule {
    MLeft | MRight | MName | MValue
  }

  def MLeft = rule {
    AssignmentName ~ WS ~ '*' ~ WS ~ Value ~> AssignmentCoefficient
  }

  def MRight = rule {
    Value ~ WS ~ '*' ~ WS ~ AssignmentName ~> ((v, n) => AssignmentCoefficient(n, v))
  }

  def MValue = rule {
    Value ~> ConstantCoefficient
  }

  def MName = rule {
    AssignmentName ~> (n => AssignmentCoefficient(n, 1))
  }

  /**
   * Captures a `Seq[String]` as appearing in the list.
   */
  def AssignmentList = rule {
    ('(' ~ WS ~ (AssignmentName + (WS ~ ',' ~ WS)) ~ WS ~ ')') 
  }

  /**
   * Caputres the name of the assignment.
   */
  def AssignmentName = rule { 
    capture(
      (CharPredicate.Alpha | ch('_')) ~ 
      zeroOrMore(CharPredicate.Alpha | ch('_') | CharPredicate.Digit)
    )
  }

  def Value = rule {
    HexValue | TenValue
  }

  def HexValue = rule {
    ignoreCase("0x") ~ capture(oneOrMore(CharPredicate.HexDigit)) ~> (i => BigInt(new BigInteger(i, 16)))
  }

  def TenValue = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> (i => BigInt(new BigInteger(i, 10)))
  }

  def EqualsT = rule { '=' }
  def InputT = rule { atomic("input") }
  def GateT = rule { atomic("gate") }

  def Space = rule { anyOf(" \t\n\r") }

  def WSM = rule { oneOrMore(Space) }
  def WS = rule { quiet(zeroOrMore(Space)) }
}
