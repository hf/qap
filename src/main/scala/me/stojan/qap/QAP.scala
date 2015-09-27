package me.stojan.qap

import me.stojan.reunion.structure.Field

import me.stojan.polynome.Polynomial
import me.stojan.reunion.polynomials.FieldPolynomial

/**
 * Represents a Quadratic Arithmetic Program's multiplicative subcircuit.
 */
trait QAP[V] {
  /**
   * Polynomial alias type.
   */
  type P = Polynomial[Field[V]]

  def fieldDescriptor: FieldDescriptor[V]

  /**
   * The number of inputs expected for a QAP. If this number is less than 0
   * then the inputs may vary.
   *
   * Arity of 0 is valid.
   */
  def arity: Int

  /**
   * Left input polynomials, also called v.
   */
  def left: (P, Seq[P], P)

  /**
   * Right input polynomials, also called w.
   */
  def right: (P, Seq[P], P)

  def output: (P, Seq[P], P)
}

/**
 * A composite QAP is a multiplicative subcircuit that depends on the outputs of
 * two other QAP multiplicative subcircuits.
 */
trait CompositeQAP[V] extends QAP[V] {
  /**
   * Subcircuits whose output is this subcircuit's inputs. This predicate must
   * hold: `arity > 0 && inputs.size == arity`.
   */
  def inputs: Seq[QAP[V]]

  def output: (P, Seq[P], P) = (
    FieldPolynomial[V](),
    inputs.map(_ => FieldPolynomial[V]()),
    FieldPolynomial(0, fieldDescriptor.one))
}

