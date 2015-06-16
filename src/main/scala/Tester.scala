import com.github.tototoshi.csv._

import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.cluster.HierarchicalCluster._
import com.devdiscoveries.ml.cluster.KMeansCuster._
import com.devdiscoveries.ml.cluster.HiTreeNode
import com.devdiscoveries.ml.cluster.Leaf
import breeze.linalg.DenseVector
import com.devdiscoveries.ml.cluster.Data

object Tester extends App {

  val reader = CSVReader.open("./src/main/scala/blogdata.txt")(new TSVFormat {})
  val is = reader.toStream

  hclust

  def kmclust(): Unit = {
    val (features, data) = buildClusterData(is, { d => new Data(d.head, getVector(d.tail)) })
    val clust = kMeansCluster(data, 10)
    clust.foreach { c =>
      println("cluster:")
      c._2.foreach(x => println("  - " + x.id))
    }
  }

  def hclust(): Unit = {
    val (features, data) = buildClusterData(is, { d => new Leaf(d.head, getVector(d.tail)) })
    val clust = hcluster(data)
    println(printCluster(clust))
  }

  def buildClusterData[T](input: Stream[List[String]], f: List[String] => T): (List[String], List[T]) = {
    val features = input.head.tail
    val data = input.tail.map(f(_)).toList
    (features, data)
  }

  def getVector(input: List[String]): DenseVector[Double] = {
    val nums = input.map(i => i.toDouble)
    DenseVector[Double](nums.toArray)
  }

}
