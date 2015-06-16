package com.devdiscoveries.ml.functions

class FMinCG(val maxIter: Int = 100) {

  val RHO = 0.01 // a bunch of constants for line searches
  val SIG = 0.5 // RHO and SIG are the constants in the Wolfe-Powell conditions
  val INT = 0.1 // don't reevaluate within 0.1 of the limit of the current bracket
  val EXT = 3.0 // extrapolate maximum 3 times the current bracket
  val MAX = 20 // max 20 function evaluations per line search
  val RATIO = 100 // maximum allowed slope ratio

}