package me.stojan.qap.concrete

import me.stojan.qap.{ QAP, FieldDescriptor }

import me.stojan.reunion.structure.Field
import me.stojan.reunion.polynomials.FieldPolynomial

/**
 * A QAP multiplicative subcircuit that emits a value (think literal / constant).
 */
case class ValueQAP[V](fieldDescriptor: FieldDescriptor[V], value: Field[V]) extends QAP[V] {
  val arity: Int = 0

  val left: (P, Seq[P], P) = (
    FieldPolynomial(0, value),
    Seq(),
    FieldPolynomial[V]())

  val right: (P, Seq[P], P) = (
    FieldPolynomial(0, fieldDescriptor.one),
    Seq(),
    FieldPolynomial[V]())

  val output: (P, Seq[P], P) = (
    FieldPolynomial[V](),
    Seq(),
    FieldPolynomial(0, fieldDescriptor.one))
}
