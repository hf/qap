package me.stojan.qap

import me.stojan.reunion.structure.Field

/**
 * A field descriptor describes the finite field.
 */
trait FieldDescriptor[V] {
  /**
   * Returns the additive identity element of the field.
   */
  def zero: Field[V]

  /**
   * Returns the multiplicative identity element of the field.
   */
  def one: Field[V]

  /**
   * Wraps the value `v` as a field element.
   */
  def value(v: V): Field[V]
}
