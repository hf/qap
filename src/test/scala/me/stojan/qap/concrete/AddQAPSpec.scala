package me.stojan.qap.concrete

import me.stojan.qap.{ QAP, QAPSpec }

import me.stojan.reunion.structure.Field
import me.stojan.reunion.structure.concrete.PrimeField

import me.stojan.polynome.Polynomial
import me.stojan.reunion.polynomials.FieldPolynomial

class AddQAPSpec extends QAPSpec {
  import QAPUtils._

  val field = PrimeFieldDescriptor(11)

  "Add QAP" should "be a multiplicative subcircuit s.t. v_i(x) * w_i(x) - y_i(x) = 0" in {
    val qaps = Seq(ValueQAP(field, field.value(1)), ValueQAP(field, field.value(2)))
    val assignments = Seq(field.value(1), field.value(2), field.value(3))

    assertIsQAP(AddQAP(field, qaps), assignments)
  }
}
