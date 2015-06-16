package com.devdiscoveries.ml.cluster

import scala.annotation.tailrec
import com.devdiscoveries.ml.Predef._
import com.devdiscoveries.ml.functions.CorrelationFunctions._
import com.devdiscoveries.ml.cluster.HierarchicalCluster._

/**
 * Describes a general node from a hierarchical cluster tree.
 *
 * A general node in a hierarchical cluster is described by an id, containing a
 * description of the node, and a vector containing the features of the node.
 *
 * @see Leaf
 * @see Branch
 */
sealed trait HiTreeNode {
  /**
   * Returns the id that identifies the node.
   * @return a {@link String} containing the id.
   */
  def id(): String
  /**
   * Returns a vector containing the features of the node.
   * @return a {@link DblVector} containing the features as {@link Double} values.
   */
  def vec(): DblVector
}

/**
 * Describes a leaf node in a hierarchical cluster tree.
 *
 * A leaf node is described by its id and a feature vector.
 *
 * @constructor takes the id and the vector.
 */
case class Leaf(
  override val id: String,
  override val vec: DblVector) extends HiTreeNode {

  override def toString(): String = id
}

/**
 * Describes a branch node in a hierarchical cluster tree.
 *
 * A branch node is defined by a left and a right node, which are
 * the children.
 * The branch node also has the distance between its children, which
 * is calculated on construction with the fdistance function.
 *
 * The feature vector is the average of the vectors of its children and
 * the id is a concatenation of the ids of its children. These are both
 * built up at construction.
 *
 * @constructor takes the left and right nodes and a function to
 * calculate the distance between the two sub nodes.
 */
case class Branch(
  val left: HiTreeNode,
  val right: HiTreeNode,
  val fdistance: CorrelationFunction = reversePearson) extends HiTreeNode {
  val distance = fdistance(left.vec, right.vec)
  val vec = (left.vec + right.vec) / 2.0
  val id = "[" + left.id + "] - [" + right.id + "]"

  override def toString(): String = {
    "- " + left + "\n  " + right
  }
}

object HierarchicalCluster {

  def hcluster(nodes: List[HiTreeNode], fdistance: CorrelationFunction = reversePearson): HiTreeNode = hcluster(nodes, Map(), fdistance) 

  @tailrec
  private def hcluster(nodes: List[HiTreeNode], distancesCache: Map[(HiTreeNode, HiTreeNode), Double], fdistance: CorrelationFunction): HiTreeNode = {
    if (nodes.size <= 1) nodes.head
    else {
      var newDistancesCache = distancesCache
      @tailrec
      def calcDistances(ns: List[HiTreeNode], distances: List[((HiTreeNode, HiTreeNode), Double)]): List[((HiTreeNode, HiTreeNode), Double)] = {
        if (ns.size <= 1) distances
        else {
          val hn = ns.head
          ns.tail.foreach { n =>
            if (!newDistancesCache.contains((hn, n)))
              newDistancesCache = newDistancesCache + ((hn, n) -> fdistance(hn.vec, n.vec))
          }
          calcDistances(ns.tail, ns.tail.map(n => ((hn, n), newDistancesCache((hn, n)))) ++ distances)
        }
      }
      val distances = calcDistances(nodes, List())
      val lowest = distances.sortBy(e => e._2).head._1
      val newLeafs = new Branch(lowest._1, lowest._2) ::
        nodes.filter(e => e != lowest._1 && e != lowest._2)
      hcluster(newLeafs, newDistancesCache, fdistance)
    }
  }

  def printCluster(clust: HiTreeNode): String = {
    def printNode(node: HiTreeNode, prefix: String): String = {
      node match {
        case b: Branch => prefix + "= \n" + printNode(b.left, "  " + prefix) + "\n" +
          printNode(b.right, "  " + prefix) + "\n"
        case l: Leaf => prefix + "- " + l.id
      }
    }
    printNode(clust, "")
  }

}