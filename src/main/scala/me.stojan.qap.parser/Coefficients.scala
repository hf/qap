package me.stojan.qap.parser

/**
 * A coefficient adjacent to an assignment.
 */
sealed trait Coefficient {
  /**
   * Value of the coefficient.
   */
  def value: BigInt
}

/**
 * A coefficient adjacent to a named assignment, i.e.
 * `assignment * 3`.
 */
case class AssignmentCoefficient(name: String, value: BigInt) extends Coefficient

/**
 * A coefficient adjacent to an implicit assignment that
 * always is 1.
 */
case class ConstantCoefficient(value: BigInt) extends Coefficient

