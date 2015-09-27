package me.stojan.qap.concrete

import me.stojan.qap.QAPSpec

import me.stojan.reunion.structure.Field
import me.stojan.reunion.structure.concrete.PrimeField

import me.stojan.polynome.Polynomial
import me.stojan.reunion.polynomials.FieldPolynomial

class ValueQAPSpec extends QAPSpec {
  import QAPUtils._

  val field = PrimeFieldDescriptor(11)

  "Value QAP" should "be a multiplicative subcircuit s.t. v_i(x) * w_i(x) - y_i(x) = 0" in {
    val a = ValueQAP(field, field.value(1))

    ((a.left._1 + sum(a.left._2) + a.left._3) * (a.right._1 + sum(a.right._2) + a.right._3) - (a.output._1 + sum(a.output._2) + a.output._3)) should be (FieldPolynomial())
  }

  it should "emit the value on the left" in {
    val a = ValueQAP(field, field.value(2))

    a.left._1 should be (FieldPolynomial(0, field.value(2)))
    a.left._2.isEmpty should be (true)
    a.left._3 should be (FieldPolynomial())

    a.right._1 should be (FieldPolynomial(0, field.one))
    a.right._2.isEmpty should be (true)
    a.right._3 should be (FieldPolynomial())
  }

  it should "have y_0(x) = 0, y_1(x) = 1" in {
    val a = ValueQAP(field, field.value(3))

    a.output._1 should be (FieldPolynomial())
    a.output._2.isEmpty should be (true)
    a.output._3 should be (FieldPolynomial(0, field.one))
  }

  it should "have an arity of 0" in {
    val a = ValueQAP(field, field.value(7))

    a.arity should be (0)
  }
}
