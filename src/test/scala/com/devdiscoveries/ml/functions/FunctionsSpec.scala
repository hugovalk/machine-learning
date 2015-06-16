package com.devdiscoveries.ml.functions

import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.UnitSpec
import com.devdiscoveries.ml.functions.Functions._
import breeze.linalg._
import org.scalatest.Succeeded
import org.scalatest.Failed

class FunctionsSpec extends UnitSpec {
  
  "Add ones" should "add a column of 1's to a matrix" in {
    val m = DenseMatrix.zeros[Double](2,2)
    val mOnes = addOnes(m)
    mOnes.rows should be (m.rows)
    mOnes.cols should be (m.cols + 1)
    mOnes(0,0) should be (1)
    mOnes(1,0) should be (1)
    mOnes(::, 1 to 2).equals(m) should be (true)
  }
  
  "vectorToMatrixOfUnitVectors" should "convert a list of cases to a matrix of unit vectors" in {
    val l: DblVector = List(3.0,2.0,4.0)
    val m = vectorToMatrixOfUnitVectors(l, 5)
    m.rows should be (3)
    m(0,::).t should be (DenseVector(0.0,0.0,1.0,0.0,0.0))
    m(1,::).t should be (DenseVector(0.0,1.0,0.0,0.0,0.0)) 
    m(2,::).t should be (DenseVector(0.0,0.0,0.0,1.0,0.0)) 
  }
  
  it should "fail when a label larger than the numberOfLabels is supplied" in {
    val l: DblVector = List(3.0,2.0,4.0)
    try {
      vectorToMatrixOfUnitVectors(l, 3)
      fail("IllegalStateException was expected")
    } catch{ 
      case ise: IllegalStateException => Succeeded
      case _: Throwable => fail("IllegalStateException was expected")
    }
  }
  
  it should "fail when a label smaller than 1 is supplied" in {
    val l: DblVector = List(3.0,0.0,4.0)
    try {
      vectorToMatrixOfUnitVectors(l, 5)
      fail("IllegalStateException was expected")
    } catch{ 
      case ise: IllegalStateException => Succeeded
      case _: Throwable => fail("IllegalStateException was expected")
    }
  }

}