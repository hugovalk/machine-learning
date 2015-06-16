package com.devdiscoveries.ml.genalg.temp

import scala.util.Random
import scala.collection.mutable.ListBuffer

trait XoverPolicy[T] {

  def xover(parents: (T, T), rGen: Random): (T, T) = {
    if (rGen.nextDouble < xoverThreshold)
      parents
    else
      xover(parents, rGen.nextInt(2))
  }

  def xover(parents: (T, T), index: Int): (T, T)
  def xoverThreshold: Double
}

trait MutationPolicy[T] {
  def mutate(chromosomes: (T, T), rGen: Random): (T, T)
}

trait SelectionPolicy[T] {
  def select(list: ListBuffer[Chromosome[T]], rGen: Random): (T, T)
}

