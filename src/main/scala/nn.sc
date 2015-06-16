import com.devdiscoveries.ml.neuralnet.NeuralNetwork
import breeze.linalg._
import breeze.io._
import com.devdiscoveries.ml.functions.Functions._

object scratch {
//	val nn = NeuralNetwork(3,3,3,4)
//  DenseVector.zeros[Double](3)
  
//	nn.asciiArt

	def testLinRegCostFunction(): Double = {
		//val input = csvread(new java.io.File("/Users/hugovalk/Dropbox/dev/octave/machine-learning/mlclass-ex1-005/mlclass-ex1/ex1data1.txt"))
		val input = csvread(new java.io.File("C:/Dropbox/dev/octave/machine-learning/mlclass-ex1-005/mlclass-ex1/ex1data1.txt"))
	  val X = input(::,0).toDenseMatrix.t
	  val y = input(::,1)
	  val theta = DenseVector.zeros[Double](2)
	  linRegCostFunction(X, y, theta)
	}
  
  def testLogRegCostFunction(): Double = {
	  //val input = csvread(new java.io.File("/Users/hugovalk/Dropbox/dev/octave/machine-learning/mlclass-ex2-005/mlclass-ex2/ex2data1.txt"))
	  val input = csvread(new java.io.File("C:/Dropbox/dev/octave/machine-learning/mlclass-ex2-005/mlclass-ex2/ex2data1.txt"))
	  val X = input(::,0).toDenseMatrix.t
	  val y = input(::,1)
	  val theta = DenseVector.zeros[Double](2)
	  val regTheta = DenseVector.vertcat(DenseVector.zeros[Double](1), theta.slice(1, theta.size, 1))
	  logRegCostFunction(X, y, theta, 0)
  }
  
//  testLinRegCostFunction()
//	testLogRegCostFunction()
}