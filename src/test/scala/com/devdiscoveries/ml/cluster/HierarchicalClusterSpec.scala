package com.devdiscoveries.ml.cluster

import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.UnitSpec
import HierarchicalCluster._
import com.devdiscoveries.ml.functions.CorrelationFunctions._
import breeze.linalg.DenseVector
import org.scalatest.Succeeded
import org.scalatest.Failed

class HierarchicalClusterSpec extends UnitSpec {

  "Hierarchical clustering" should "return the same leaf when there is only one." in {
    val leaf = new Leaf("11", List(1, 0, 2.0, 3.0))
    val clust = hcluster(List(leaf))
    clust should equal(leaf)
    clust.id should be("11")
  }

  it should "return one branch when two leafs are provided" in {
    val leaf1 = new Leaf("11", List(1, 0, 2.0, 3.0))
    val leaf2 = new Leaf("12", List(2, 0, 3.0, 4.0))
    val clust = hcluster(List(leaf1, leaf2))
    clust match {
      case Branch(left, right, distance) =>
        left should be(leaf1)
        right should be(leaf2)
      case _ => Failed
    }
  }

  it should "cluster the closest leafs together" in {
    val leaf1 = new Leaf("11", List(1, 0, 2.0, 3.0))
    val leaf2 = new Leaf("12", List(2, 0, 3.0, 4.0))
    val leaf3 = new Leaf("13", List(2, 0, -3.0, -8.0))
    val clust = hcluster(List(leaf1, leaf2, leaf3))
    clust match {
      case Branch(Branch(left2, right2, distance2), right, distance) =>
        left2 should be(leaf1)
        right2 should be(leaf2)
        right should be(leaf3)
      case _ => Failed
    }
  }

}