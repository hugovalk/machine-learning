package com.devdiscoveries.ml.functions

import breeze.linalg._
import breeze.stats._
import scala.math._
import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.neuralnet.NeuralNetwork

object Functions {

  def logistic(x: Double): Double = 1 / (1 + exp(-1 * x))
  def logistic(x: DblVector): DblVector = x.map(logistic(_))
  def logistic(x: DblMatrix): DblMatrix = x.map(logistic(_))

  def logisticGradient(x: Double): Double = logistic(x) * (1 - logistic(x))
  def logisticGradient(x: DblVector): DblVector = x.map(logisticGradient(_))
  def logisticGradient(x: DblMatrix): DblMatrix = x.map(logisticGradient(_))

  def logRegCostFunction(X: DblMatrix, y: DblVector, theta: DblVector): Double =
    logRegCostFunction(X, y, theta, 0.0)

  def logRegCostFunction(X: DblMatrix, y: DblVector, theta: DblVector, lambda: Double): Double = {
    val h: DblVector = logistic(addOnes(X) * theta)
    val regTheta = theta.copy
    regTheta.update(0, 0.0)
    val s = y.map(_ * -1).t * h.map(log(_)) - y.map(1 - _).t * h.map(elem => log(1 - elem))
    (1.0 / y.size) * (s + lambda * (regTheta.t * regTheta))
  }

  def linRegCostFunction(X: DblMatrix, y: DblVector, theta: DblVector): Double =
    linRegCostFunction(X, y, theta, 0.0)

  def linRegCostFunction(X: DblMatrix, y: DblVector, theta: DblVector, lambda: Double): Double = {
    val z = (addOnes(X) * theta) - y
    val regTheta = theta.copy
    regTheta.update(0, 0.0)
    (1.0 / (2.0 * y.size)) * ((z.t * z) + lambda * (regTheta.t * regTheta));
  }

  def addOne(v: DblVector): DblVector = 
    DenseVector.vertcat(List(1.0), v)

  def addZero(v: DblVector): DblVector = 
    DenseVector.vertcat(List(0.0), v)
    
  def addOnes(X: DblMatrix): DblMatrix =
    DenseMatrix.horzcat(DenseMatrix.ones[Double](X.rows, 1), X)
  
  def addZeros(X: DblMatrix): DblMatrix =
    DenseMatrix.horzcat(DenseMatrix.zeros[Double](X.rows, 1), X)
    
  def featureNormalize(X: DblMatrix): DblMatrix = {
    val means = mean(X(::,*)).toDenseVector
    val stddevs = stddev(X(::,*)).toDenseVector
    val invstd = DenseVector.ones[Double](stddevs.size).:/=(stddevs)
    (X(*,::).:-=(means)).:*=(invstd)
    X
  }
  
  def vectorToMatrixOfUnitVectors(input: DblVector, numberOfLabels: Int): DblMatrix = {
    val I = DenseMatrix.eye[Double](numberOfLabels)
    var result = DenseMatrix.create[Double](0, numberOfLabels, Array())
    input.foreach { d => 
      if (d >= 1.0 && d <= numberOfLabels)
        result = DenseMatrix.vertcat(result, I(d.intValue() - 1,::).t.asDenseMatrix)
        else 
          throw new IllegalStateException("Labels are 1-based, therefore 0 is not allowed in input and only values <= than numberOfLabels are allowed.")}
    result
  }
}
