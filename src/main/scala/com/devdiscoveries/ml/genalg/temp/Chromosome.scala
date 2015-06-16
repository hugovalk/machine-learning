package com.devdiscoveries.ml.genalg.temp

import scala.collection.mutable.ArrayBuffer

abstract class Chromosome[T](val len: Int) {
  var representation = new ArrayBuffer[T](len)

  /**
   * The fitness of the chromosome.
   */
  def evaluate: Double

  /**
   * Compare this chromosome to the other, by using the
   * evaluate function.
   */
  def compareTo(other: Chromosome[T]): Boolean = {
    evaluate > other.evaluate
  }

  def get(index: Int): T = representation(index)

  def set(el: T, index: Int): Unit = representation(index) = el

  def add(el: T, index: Int): Unit = representation += el.asInstanceOf[T]

}