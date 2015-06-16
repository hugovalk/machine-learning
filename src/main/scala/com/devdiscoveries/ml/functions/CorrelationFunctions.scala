package com.devdiscoveries.ml.functions

import breeze.linalg.DenseVector
import breeze.linalg._
import breeze.numerics._
import com.devdiscoveries.ml.Predef._

object CorrelationFunctions {

  def reversePearson(v1: DblVector, v2: DblVector): Double =
    1.0 - pearson(v1, v2)

  def pearson(v1: DblVector, v2: DblVector): Double = {
    val sumv1 = sum(v1)
    val sumv2 = sum(v2)
    val den = sqrt(
      (v1.t * v1 - pow(sumv1, 2) / v1.length) *
        (v2.t * v2 - pow(sumv2, 2) / v2.length))
    if (den == 0.0)
      return 0.0
    val num = v1.t * v2 - (sumv1 * sumv2 / v1.length)
    return num / den
  }

  def sumOfSquaredDifferences(v1: DblVector, v2: DblVector): Double = {
    val r = v1 - v2
    return r.t * r
  }

  def euclidDistance(v1: DblVector, v2: DblVector): Double = {
    val sum1 = sum(v1 :* v1)
    val sum2 = sum(v2 :* v2)
	  return sqrt(sum1 + sum2)
  } 
  

  //  def sim_distance(prefs,person1,person2):
  //  # Get the list of shared_items
  //  si={}
  //  for item in prefs[person1]: 
  //    if item in prefs[person2]: si[item]=1
  //
  //  # if they have no ratings in common, return 0
  //  if len(si)==0: return 0
  //
  //  # Add up the squares of all the differences
  //  sum_of_squares=sum([pow(prefs[person1][item]-prefs[person2][item],2) 
  //                      for item in prefs[person1] if item in prefs[person2]])
  //
  //  return 1/(1+sum_of_squares)

}