package com.devdiscoveries.ml.genalg

import scala.util.Random

object GenAlgSolver {

  type Population[T] = List[T]
  type Fitness = Double
  type FitnessFunction[T] = T => Fitness
  type RecombineFunction[T] = (T, T) => (T, T)
  type MutateFunction[T] = T => T

  def sort[T](population: Population[T], fitnessFunction: FitnessFunction[T]): Population[T] =
    population.sortBy[Double](fitnessFunction)

  def run[T](population: Population[T],
             numberOfGenerations: Int,
             parentPoolSize: Int,
             parentSelectionSize: Int,
             mutationProbability: Double,
             fitnessThreshold: Double,
             fitnessFunction: FitnessFunction[T],
             recombineFunction: RecombineFunction[T],
             mutateFunction: MutateFunction[T]): T = {
    def loop(sortedPopulation: Population[T], generation: Int): T = {
      if (generation >= numberOfGenerations || fitnessFunction(sortedPopulation.head) > fitnessThreshold)
        return sortedPopulation.head
      val selectedParents = selectParents(sortedPopulation, parentPoolSize, parentSelectionSize)
      val children = recombine(sort(selectedParents, fitnessFunction), recombineFunction)
      val mutatedChildren = mutate(children, mutationProbability, mutateFunction)
      val survivors = sort(mutatedChildren ++ sortedPopulation, fitnessFunction).take(sortedPopulation.size)
      loop(sort(survivors, fitnessFunction), generation + 1)
    }
    loop(sort(population, fitnessFunction), 0)
  }

  def selectParents[T](population: Population[T],
                       parentPoolSize: Int,
                       parentSelectionSize: Int): Population[T] = {
    def loop(population: Population[T], i: Int, result: Population[T]): Population[T] = {
      if (i >= parentPoolSize) result
      val elem = population(Random.nextInt(population.size))
      loop(population.filter(_ != elem), i + 1, result :+ elem)
    }
    loop(population, 0, List()).take(parentSelectionSize)
  }

  def recombine[T](parents: Population[T], recombineFunction: RecombineFunction[T]): Population[T] = {
    def loop(parents: Population[T], result: Population[T]): Population[T] = parents match {
      case List() => result
      case h :: _ => result
      case h1 :: h2 :: t =>
        val (c1, c2) = recombineFunction(h1, h2)
        loop(parents.tail.tail, c1 :: c2 :: parents)
    }
    loop(parents, List())
  }

  def mutate[T](children: Population[T], mutationProbability: Double, mutateFunction: MutateFunction[T]): Population[T] =
    for (child <- children) yield {
      if (Random.nextDouble <= mutationProbability)
        mutateFunction(child)
      else
        child
    }
}