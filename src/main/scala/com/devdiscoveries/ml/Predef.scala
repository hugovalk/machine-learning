package com.devdiscoveries.ml

import breeze.linalg._
import scala.reflect.ClassTag
import scala.language.implicitConversions

/**
 * All predefined types and implicit conversions for use within this project and
 * projects depending on it. 
 * 
 * Contains a number of useful types and implicit conversions to make the code less
 * verbose, especially when using the Breeze library. 
 * 
 * It is recommended to always import all its members:
 * {{{
 * import com.devdiscoveries.ml.Predef._
 * 
 * val v: DenseVector[Double] = List(1,2,3)
 * }}}  
 */
object Predef {

  /** Shorthand type for [[breeze.linalg.DenseVector]] with [[scala.Double]]s. */
  type DblVector = DenseVector[Double]
  /** Shorthand type for [[breeze.linalg.DenseMatrix]] with [[scala.Double]]s. */
  type DblMatrix = DenseMatrix[Double]
  
  /** Describes a correlation function between two vectors. */
  type CorrelationFunction = (DblVector, DblVector) => Double

  /** Converts a [[scala.collection.Seq]] to a [[breeze.linalg.DenseVector]]. */
  implicit def listToDenseVector[T: ClassTag](l: Seq[T]): DenseVector[T] =
    DenseVector[T](l.toArray)

}