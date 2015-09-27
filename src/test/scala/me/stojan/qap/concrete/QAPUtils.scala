package me.stojan.qap.concrete

import me.stojan.qap.QAP

import me.stojan.reunion.structure.Field

import me.stojan.polynome.Polynomial

import me.stojan.reunion.polynomials.FieldPolynomial

object QAPUtils {
  /**
   * Sums up a sequence of polynomials.
   */
  def sum[V](s: Seq[Polynomial[Field[V]]]): Polynomial[Field[V]] =
    if (s.isEmpty) {
      FieldPolynomial()
    } else {
      s.tail.fold(s.head) { (i, a) => i + a }
    }

  def sum[V](s: (Polynomial[Field[V]], Seq[Polynomial[Field[V]]], Polynomial[Field[V]])): Polynomial[Field[V]] =
    s._1 + sum(s._2) + s._3

  def dot[V](s: Seq[Polynomial[Field[V]]], a: Seq[Field[V]]): Polynomial[Field[V]] =
    sum(a.map(f => FieldPolynomial[V](0, f)).zip(s).map { i: (Polynomial[Field[V]], Polynomial[Field[V]]) => i._1 * i._2 })

  def assertIsQAP[V](qap: QAP[V], assignments: Seq[Field[V]]): Unit = {
    import org.scalatest.Assertions._

    val left = qap.left._1 + dot(qap.left._2 :+ qap.left._3, assignments)
    val right = qap.right._1 + dot(qap.right._2 :+ qap.right._3, assignments)
    val output = qap.output._1 + dot(qap.output._2 :+ qap.output._3, assignments)

    assert((left * right - output) == FieldPolynomial())
  }
}
