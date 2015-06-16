package com.devdiscoveries.ml.cluster

import com.devdiscoveries.ml.UnitSpec
import breeze.linalg.DenseVector
import com.devdiscoveries.ml.cluster.KMeansCuster._

class KMeansClusterSpec extends UnitSpec {

  "Find closest centroid" should "find the closest of two vectors to a sample vector" in {
    val data = Data("1", DenseVector[Double](1.0, 2.0, 3.0))
    val c1 = DenseVector[Double](2.0, 3.0, 4.0)
    val c2 = DenseVector[Double](1.0, 0.0, -1.0)
    findClosestCentroid(data, List(c1, c2)) should be(c1)
  }

  "K Means cluster" should "find two clusters in 5 vectors which are divided in 2 clusters for k=2" in {
    val d1 = Data("1", DenseVector[Double](0.0, -1.0, -2.0))
    val d2 = Data("2", DenseVector[Double](2.0, 3.0, 4.0))
    val d3 = Data("3", DenseVector[Double](1.0, 0.0, -1.0))
    val d4 = Data("4", DenseVector[Double](1.0, 2.0, 3.0))
    val d5 = Data("5", DenseVector[Double](3.0, 4.0, 5.0))
    val clusters = kMeansCluster(List(d1, d2, d3, d4, d5), 2)

    clusters.size < 3 should be(true)
    clusters.size > 0 should be(true)

    if (clusters.size == 2)
      clusters.foreach(c => validateClust(c._2))

    def validateClust(clust: List[Data]): Unit = {
      if (clust.contains(d1)) {
        clust.size should be(2)
        clust.contains(d3) should be(true)
      }
      if (clust.contains(d2)) {
        clust.size should be(3)
        clust.contains(d4) should be(true)
        clust.contains(d5) should be(true)
      }
    }
  }

}