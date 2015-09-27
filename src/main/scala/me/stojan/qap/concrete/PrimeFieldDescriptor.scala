package me.stojan.qap.concrete

import me.stojan.qap.FieldDescriptor

import me.stojan.reunion.structure.Field
import me.stojan.reunion.structure.concrete.PrimeField

/**
 * Descriptor for a prime finite field.
 */
case class PrimeFieldDescriptor(prime: BigInt) extends FieldDescriptor[BigInt] {
  val one: Field[BigInt] = PrimeField(prime, 1)
  val zero: Field[BigInt] = PrimeField(prime, 0)

  def value(i: BigInt): Field[BigInt] = PrimeField(prime, i)
}
