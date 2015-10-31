package me.stojan.qap.parser

/**
 * A statement.
 */
sealed trait Statement

/**
 * A QAP assignment definition.
 */
sealed trait Assignment extends Statement {
  /**
   * Name of assignment.
   */
  def name: String
}

/**
 * Assignment of the form `name = input;`.
 */
case class InputAssignment(name: String) extends Assignment

/**
 * Assignment of the form `name = 3;`.
 */
case class ConstantAssignment(name: String, value: BigInt) extends Assignment

/**
 * Assignment of the form `name = gate_name(a1, a2);`.
 */
case class GateAssignment(name: String, gate: String, assignments: Seq[String]) extends Assignment


case class GateStatement(name: String, inputs: Seq[String], left: Seq[Coefficient], right: Seq[Coefficient]) extends Statement

