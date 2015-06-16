package com.devdiscoveries.ml.functions

import com.devdiscoveries.ml.UnitSpec
import breeze.linalg.DenseVector
import com.devdiscoveries.ml.functions.CorrelationFunctions._
import breeze.util.DenseIntIndex

class CorrelationFunctionsSpec extends UnitSpec {

  "The Pearson correlation" should "return 1.0 for [1,2,3] and [2,3,4]" in {
    val v1 = DenseVector[Double](1, 2, 3)
    val v2 = DenseVector[Double](2, 3, 4)
    pearson(v1, v2) should be(1.0)
  }

  it should "return 0.9933992677987827 for [1,2,3] and [2,5,7]" in {
    val v1 = DenseVector[Double](1, 2, 3)
    val v2 = DenseVector[Double](2, 5, 7)
    pearson(v1, v2) should be(0.9933992677987827)
  }
  
  "The sum of squaredDifferences" should "return 0 when v1 == v2" in {
    val v1 = DenseVector[Double](0,5)
    sumOfSquaredDifferences(v1, v1) should be(0)
    val v2 = DenseVector[Double](5,0)
    sumOfSquaredDifferences(v2, v2) should be(0)
    val v3 = DenseVector[Double](5,5)
    sumOfSquaredDifferences(v3, v3) should be (0)
  }
  
  it should "return 18 for [2,5] and [5,2]" in {
    val v1 = DenseVector[Double](5,2)
    val v2 = DenseVector[Double](2,5)
    sumOfSquaredDifferences(v1, v2) should be(18)
  }

  "The euclidian distance" should "return 5 for [0,3] and [4,0]" in {
    val v1 = DenseVector[Double](0, 3)
    val v2 = DenseVector[Double](4, 0)
    euclidDistance(v1, v2) should be(5)
  }
}