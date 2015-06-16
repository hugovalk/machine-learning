package com.devdiscoveries.ml.genalg.temp

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Population[T] {
  var chromosomes = ListBuffer[Chromosome[T]]()

  def reject(rejectionRate: Double,
    compare: (Chromosome[T], Chromosome[T]) => Boolean): Unit = {

    chromosomes = chromosomes.sortWith(compare)
    var index = (chromosomes.size * rejectionRate).ceil.toInt
    if ((index & 0x01) != 0x01) {
      index += 1
    }
    chromosomes = chromosomes.take(index)
  }
}

abstract class Replication extends App {
  val selectionPolicy: SelectionPolicy[Int]
  val xoverPolicy: XoverPolicy[Int]
  val mutationPolicy: MutationPolicy[Int]

  def main() {

    val rGen: Random = new Random(System.currentTimeMillis())

    def extract(chromosomes: ListBuffer[Chromosome[Int]]): (Int, Int) = {
      require(chromosomes != null)

      var pair = selectionPolicy.select(chromosomes, rGen)
      pair = xoverPolicy.xover(pair, rGen)
      mutationPolicy.mutate(pair, rGen)
    }
  }
}