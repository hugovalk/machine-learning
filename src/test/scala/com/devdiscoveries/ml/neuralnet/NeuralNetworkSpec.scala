package com.devdiscoveries.ml.neuralnet

import com.devdiscoveries.ml.UnitSpec
import com.devdiscoveries.ml.Predef._
import breeze.linalg._
import org.apache.commons.math3.analysis.solvers.SecantSolver
import org.apache.commons.math3.analysis.function.Sin
import org.apache.commons.math3.analysis.function.Tan
import org.apache.commons.math3.analysis.function.Cos

class NeuralNetworkSpec extends UnitSpec {

  "A default multilayer perceptron network" should "have two weight matrices" in {
    MultiLayerPerceptronNetwork().weights.size should be(2)
  }

  it should "have 3 layers" in {
    MultiLayerPerceptronNetwork().activations(List(0.0, 0.0, 0.0)).size should be(3)
  }

  it should "have 1 output layer of size 2 in layers" in {
    MultiLayerPerceptronNetwork().activations(List(0.0, 0.0, 0.0)).head._2.size should be(2)
  }
  
  it should "have 1 hidden layer of size 3" in {
    MultiLayerPerceptronNetwork().activations(List(0.0, 0.0, 0.0)).tail.head._2.size should be(3)
  }

  it should "have an output layer of size 2" in {
    MultiLayerPerceptronNetwork().output(List(0.0, 0.0, 0.0)).size should be(2)
  }

  it should "have an output layer of size 2 for multiOutput when input matrix has 1 row" in {
    val m = DenseMatrix.create[Double](1, 3, Array(0.0, 0.0, 0.0))
    val out = MultiLayerPerceptronNetwork().multiOutput(m)
    out.rows should be(1)
    out.cols should be(2)
  }
  
  "a multilayer perceptron network" should "be able to initialize random weights" in {
    val nn = MultiLayerPerceptronNetwork(2, 4, 5, 4)
    val weight1 = nn.weights.tail.tail.head
    weight1.rows should be (4)
    weight1.cols should be (3)
    val weight2 = nn.weights.tail.head
    weight2.rows should be (5)
    weight2.cols should be (5)
    val weight3 = nn.weights.head
    weight3.rows should be (4)
    weight3.cols should be (6)
  }
  
  "a multilayer perceptron network with layers 1 2 3" should "have the first weight matrix with dimension (2,2)" in {
    val nn = MultiLayerPerceptronNetwork(1,2,3)
    nn.weights.tail.head.rows should be(2)
    nn.weights.tail.head.cols should be(2)
  }
  
  it should "have a second weight matrix with dimensions (3,3)" in {
    val nn = MultiLayerPerceptronNetwork(1,2,3)
    nn.weights.head.rows should be(3)
    nn.weights.head.cols should be(3)
  }
  
  it should "have 2 weight matrices" in {
    val nn = MultiLayerPerceptronNetwork(1,2,3)
    nn.weights.size should be(2)
  }

  
  // Prepare some default values for some test cases. 
  val cos = new Cos()
  val tan = new Tan()
  val y: DblVector = (1 to 16).map(_.doubleValue % 4 + 1).toList
  val Theta1: DblMatrix = DenseMatrix.create(4, 3, (1 to 12).map(x => 1 / cos.value(x.doubleValue)).toArray)
  val Theta2: DblMatrix = DenseMatrix.create(4, 5, (13 to 32).map(x => 1 / cos.value(x.doubleValue)).toArray)
  val X = DenseMatrix.create(16, 2, (1 to 32).map(x => tan.value(x.toDouble)).toArray).map(_ / 5.0)

  "A given Theta1, Theta2, X and y" should "return 10.93109 as the cost" in {
    val nn = MultiLayerPerceptronNetwork(List(Theta2, Theta1), 2, 4, 4)
    nn.cost(X, y, 4, 0) shouldEqual 10.93109 +- 0.00001 
  }
  
  it should "return 170.99 as the cost with lambda = 0.1 (regularization)" in {
    val nn = MultiLayerPerceptronNetwork(List(Theta2, Theta1), 2, 4, 4)
    nn.cost(X, y, 4, 0.1) shouldEqual 170.99332 +- 0.00001
  }
  
  
  it should "return the correct weight gradient matrices" in {
    val nn = MultiLayerPerceptronNetwork(List(Theta2, Theta1), 2, 4, 4)
    val expectedGradient1 = DenseMatrix((0.30518, 0.62115, -0.0029711, -0.046995),
                                        (0.071044, -0.074310, -0.055435, 0.00010499),
                                        (0.051307, 0.052173, -0.0095647, 0.0090452))
    val expectedGradient2 = DenseMatrix((-0.074506, 0.53455, -0.034384, 0.15063),
                                        (0.74997, -0.078995, 0.066441, -0.017708),
                                        (-0.017991, 0.35278, -0.034314, 0.27170),
                                        (0.44328, -0.0053284, 0.33322, 0.071129),
                                        (-0.059840, 0.084440, -0.070455, 0.14488))

    val gradients = nn.gradients(X, y, 4, 0)
    val gradient2 = List() ++ gradients.head.toArray
    val expgradient2 = List() ++ expectedGradient2.toArray
    val pairs2 = gradient2.zip(expgradient2)
    for ((g, exp) <- pairs2) yield g shouldEqual exp +- 0.00001
    
    val gradient1 = List() ++ gradients.tail.head.toArray
    val expgradient1 = List() ++ expectedGradient1.toArray
    val pairs1 = gradient1.zip(expgradient1)
    for ((g, exp) <- pairs1) yield g shouldEqual exp +- 0.00001
  }

  it should "return the correct weight gradient matrices with lambda = 0.1 (regularization)" in {
    val nn = MultiLayerPerceptronNetwork(List(Theta2, Theta1), 2, 4, 4)

    val expectedGradient1 = DenseMatrix((0.3051843, 0.6211486, 0.0053191, -0.0544438),
    									(0.0710438, -0.0522766, -0.0983900, 1.4123116),
    									(0.0513066, 0.0586827, -0.0164243, 0.0164517))
        
    val expectedGradient2 = DenseMatrix((-0.0745060, 0.5440175, -0.0461142, 0.1441408), 
		  							  (0.7499671, -0.0726739, 0.0811755, -0.0260627), 
		  							  (-0.0179905, 0.3680935, -0.0280090, 0.3122174), 
		  							  (0.4432801, -0.0167392, 0.3428785, 0.0779614),
		  							  (-0.0825542, 0.0781902, -0.0918487, 0.1523740))
  
    val gradients = nn.gradients(X, y, 4, 0.1)
    val gradient2 = List() ++ gradients.head.toArray
    val expgradient2 = List() ++ expectedGradient2.toArray
    val pairs2 = gradient2.zip(expgradient2)
    for ((g, exp) <- pairs2) yield g shouldEqual exp +- 0.00001
    
    val gradient1 = List() ++ gradients.tail.head.toArray
    val expgradient1 = List() ++ expectedGradient1.toArray
    val pairs1 = gradient1.zip(expgradient1)
    for ((g, exp) <- pairs1) yield g shouldEqual exp +- 0.00001
  }
  
  it should "not crash with an extra layer" in {
    val nn = MultiLayerPerceptronNetwork(2, 4, 5, 4)
    nn.cost(X, y, 4, 0.1)
    nn.gradients(X, y, 4, 0.1)
  }

}