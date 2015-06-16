package com.devdiscoveries.ml.neuralnet

import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.functions.Functions._
import breeze.linalg._
import breeze.numerics._

sealed trait NeuralNetwork {
  /**
   * @return the size of the input layer.
   */
  def sizeInput: Int
  /**
   * @return the size of the output layer.
   */
  def sizeOutput: Int
  /**
   * Returns the sizes of the hidden layers. The first element is the size of
   * the hidden layer next to the input layer, the last element is the size of the
   * hidden layer next to the output layer.
   *
   * @return the sizes of the hidden layers.
   */
  def sizesHiddenLayers: Seq[Int]
  def weights: List[DblMatrix]
  def multiOutput(input: DblMatrix): DblMatrix
  def output(input: DblVector): DblVector
}

case class MultiLayerPerceptronNetwork(
  override val sizeInput: Int = 3,
  override val sizeOutput: Int = 2,
  // TODO: Create val layers and calculate sizeInput, sizeOutput and sizesHiddenLayers
  override val sizesHiddenLayers: Seq[Int] = List(3),
  val initialWeights: List[DblMatrix] = List()) extends NeuralNetwork {
  override val weights = if (initialWeights.isEmpty) initWeights() else initialWeights

  private def initWeights(): List[DblMatrix] = {
    def iter(hl: List[Int], ws: List[DblMatrix]): List[DblMatrix] = hl match {
      case e :: List() => DenseMatrix.rand[Double](sizeOutput, e + 1) :: ws
      case e :: es     => iter(es, DenseMatrix.rand[Double](es.head, e + 1) :: ws)
      case List()      => throw new IllegalStateException("There must be at least 1 hidden layer. ")
    }
    iter(sizesHiddenLayers.toList, List(DenseMatrix.rand[Double](sizesHiddenLayers.head, sizeInput + 1)))
  }

  def train(input: DblMatrix, output: DblVector, lambda: Double) = ???

  /**
   * Calculates the activations for each layer.
   * @param input an input vector.
   *
   * @return a List of vector pairs, containing the z and a values.
   */
  def activations(input: DblVector): List[(DblVector, DblVector)] = {
    def iter(result: List[(DblVector, DblVector)], ws: List[DblMatrix]): List[(DblVector, DblVector)] = {
      ws match {
        case List() => result
        case w :: ws =>
          val z = w * addOne(result.head._2)
          val newResult = (z, logistic(z))
          iter(newResult :: result, ws)
      }
    }
    val inputPair = (DenseVector.zeros[Double](input.length), input)
    iter(List(inputPair), weights.reverse)
  }

  /**
   * Calculates the output of multiple input vectors (contained in the input matrix.
   *
   * @param input a matrix containing all input vectors.
   *
   * @return a matrix containing the results of all input vectors.
   */
  override def multiOutput(input: DblMatrix): DblMatrix = {
    val rowVectors = for {
      i <- 0 until input.rows
    } yield (output(input(i, ::).t))
    var res = DenseMatrix.create(1, rowVectors.head.size, rowVectors.head.data)
    if (rowVectors.size > 1)
      rowVectors.tail.foreach(row => res = DenseMatrix.vertcat(res, row.toDenseMatrix))
    res
  }

  override def output(input: DblVector): DblVector = activations(input).head._2

  /**
   * For a set of input values and a set of expected outputs, this
   * function will calculate the cost.
   *
   * @param input           A matrix containing the input values.
   * @param output          A vector containing the expected output values.
   * @param numberOfLabels  the number of labels used for classification.
   *
   * @return The cost.
   */
  def cost(input: DblMatrix, output: DblVector, numberOfLabels: Int, lambda: Double): Double = {
    val X = addOnes(input)
    val m = X.rows
    val Yv = vectorToMatrixOfUnitVectors(output, numberOfLabels)
    val h = multiOutput(input)
    val dJs = for {
      i <- 0 until output.size
    } yield ((log(h(i, ::).t).t * Yv(i, ::).t) * -1 -
      log((h(i, ::).t.map(1 - _))).t * (Yv(i, ::).t.map(1 - _)))

    val sums = for {
      theta <- weights.reverse
    } yield {
      val t2 = theta(::, 1 to -1).map(pow(_, 2.0))
      sum(sum(t2, Axis._0))
    }

    val reg = (lambda / (2 * m)) * sums.foldLeft(0.0)(_ + _)

    dJs.foldLeft(0.0)(_ + _) / m + reg
  }

  /**
   * For a set of input values and a set of expected outputs, this
   * function will calculate the weight gradients.
   *
   * @param input           A matrix containing the input values.
   * @param output          A vector containing the expected output values.
   * @param numberOfLabels  the number of labels used for classification.
   *
   * @return The weight gradients.
   */
  def gradients(input: DblMatrix, output: DblVector, numberOfLabels: Int, lambda: Double): List[DblMatrix] = {
    val Yv = vectorToMatrixOfUnitVectors(output, numberOfLabels)

    val lDeltas = for {
      i <- 0 until output.size
    } yield {
      val z_as = activations(input(i, ::).t)
      def calcDs(curWeights: List[DblMatrix], curActivations: List[(DblVector, DblVector)], result: List[DblVector]): List[DblVector] = curWeights match {
        case w :: w2 :: ws =>
          val d_1 = (w.t * result.head)
          val d = d_1 :* DenseVector.vertcat(List(0.0), logisticGradient(curActivations.head._1))
          calcDs(curWeights.tail, curActivations.tail, d(1 to -1) :: result)
        case _ => result
      }
      val dOut = z_as.head._2 - Yv(i, ::).t
      val ds = calcDs(weights, z_as.tail, List(dOut)).reverse
      def calcDeltas(curDs: List[DblVector], curActivations: List[(DblVector, DblVector)], result: List[DblMatrix]): List[DblMatrix] = curActivations match {
        case a :: as =>
          val delta = curDs.head * addOne(a._2).t
          calcDeltas(curDs.tail, curActivations.tail, delta :: result)
        case List() => result
      }
      val deltaOut = ds.head * addOne(z_as.tail.head._2).t
      val deltas = calcDeltas(ds.tail, z_as.tail.tail, List(deltaOut))
      deltas
    }
    val zMs = lDeltas.head.map(m => DenseMatrix.zeros[Double](m.rows, m.cols))
    val res = lDeltas.foldLeft(zMs) { (m, zm) =>
      m.zip(zm).map(mzm => mzm._1 + mzm._2)
    }.toList
    val regs = weights.map(w => addZeros(w(::, 1 to -1)))
    val unreg = res.map(_.map(e => e * (1.0 / output.size)))
    unreg.reverse.zip(regs).map(e => e._1 + (e._2 :* (lambda / output.size)))
  }

}

object MultiLayerPerceptronNetwork {

  def apply(): MultiLayerPerceptronNetwork = new MultiLayerPerceptronNetwork()

  def apply(layerSizes: Int*): MultiLayerPerceptronNetwork =
    apply(List(), layerSizes: _*)

  def apply(initialWeights: List[DblMatrix], layerSizes: Int*): MultiLayerPerceptronNetwork = {
    if (layerSizes.size < 3) throw new IllegalArgumentException("A network must have at least 3 layers.")
    val input = layerSizes.head
    val output = layerSizes.reverse.head
    new MultiLayerPerceptronNetwork(sizeInput = input,
      sizeOutput = output,
      sizesHiddenLayers = layerSizes.tail.reverse.tail.reverse,
      initialWeights = initialWeights)
  }

}