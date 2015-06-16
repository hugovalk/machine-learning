package com.devdiscoveries.ml.cluster

import com.devdiscoveries.ml.functions.CorrelationFunctions._
import com.devdiscoveries.ml.Predef._
import scala.util.Random
import breeze.linalg.DenseVector

case class Data(val id: String, val vec: DblVector) {}

object KMeansCuster {
  def findClosestCentroid(row: Data,
    centroids: List[DblVector],
    fdistance: CorrelationFunction = reversePearson): DblVector = {
    centroids.map(c => (c, fdistance(row.vec, c))).sortBy(_._2).head._1
  }

  def kMeansCluster(rows: List[Data], numClusters: Int, fdistance: CorrelationFunction = reversePearson): List[(DblVector, List[Data])] = {
    val centroids = Random.shuffle(rows).take(numClusters).map(_.vec)

    def iterate(centroids: List[DblVector]): List[(DblVector, List[Data])] = {
      val closestCentroids = rows.map(r => (r, findClosestCentroid(r, centroids, fdistance)))

      val cl_cent = centroids.map(c => (c, closestCentroids.filter(_._2 == c).map(_._1)))
        .filter(_._2.size != 0)
      val newCentroids = cl_cent.map { c =>
        val sum = c._2.map(_.vec).foldLeft(DenseVector.zeros[Double](c._2.head.vec.size))(_ + _)
        val result = sum / (c._2.size.toDouble)
        (result, c._2)
      }
      if (!newCentroids.map(_._1).equals(centroids))
        iterate(newCentroids.map(_._1))
      else
        newCentroids
    }
    iterate(centroids)
  }

}

