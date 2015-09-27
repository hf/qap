package me.stojan.qap.concrete

import me.stojan.qap.{ QAP, CompositeQAP, FieldDescriptor }

import me.stojan.reunion.polynomials.FieldPolynomial

/**
 * A QAP multiplicative subcircuit that adds the outputs of two other QAP
 * multiplicative subcircuits.
 */
case class AddQAP[V](fieldDescriptor: FieldDescriptor[V], inputs: Seq[QAP[V]]) extends CompositeQAP[V] {
  val arity: Int = -1

  val left: (P, Seq[P], P) = (
    FieldPolynomial[V](),
    inputs.map(_ => FieldPolynomial(0, fieldDescriptor.one)),
    FieldPolynomial[V]())

  val right: (P, Seq[P], P) = (
    FieldPolynomial(0, fieldDescriptor.one),
    inputs.map(_ => FieldPolynomial[V]()),
    FieldPolynomial[V]())
}
