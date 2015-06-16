//package com.devdiscoveries.ml.genalg genalg {
//
//  type Gene[T] = List[T]
//  type Chromosome[T] = List[T]
//  type GenePool[T] = List[T]
//  type Fitness[T] = Chromosome[T] => Double
//
//  implicit def genesToChromosome[T](genes: List[Gene[T]]): Chromosome[T] =
//    genes.flatten
//
//}