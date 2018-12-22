package Day08
import scala.io.Source


class Day08 {

  case class Node(children: Array[Node], metadata: Array[Int]) {
    val getMetadataSum: Int = metadata.sum + children.map(_.getMetadataSum).sum

    val getValue: Int = {
      if (children.length == 0) {
        getMetadataSum
      }
      else {
        metadata.map { m => if (children.length - 1 >= m - 1) children(m - 1).getValue else 0 }.sum
      }
    }

    val getSize: Int = {
      if (children.length == 0) {
        2 + metadata.length
      }

      2 + metadata.length + children.map(_.getSize).sum
    }
  }

  def parseNode(input: Array[Int]): Node = {
    // Parse head of node
    val childrenSize = input(0)
    val metadataSize = input(1)

    // Parse rest of current node (which is also a tree)
    val children = parseTree(input.slice(2, input.length), childrenSize)

    // Get children size to figure out where is the metadata
    val childrenLength = children.foldLeft(0)((sum, n) => sum + n.getSize)

    // Fetch metadata from the input
    val metadata = input.slice(2 + childrenLength, 2 + childrenLength + metadataSize)

    // Return the node now that we know its children and metadata
    Node(children, metadata)
  }

  def parseTree(input: Array[Int], children: Int): Array[Node] = {
    // If it has no more children, return empty
    if (children == 0) {
      Array.empty
    }
    // Parse a child and then to parse the rest of the tree
    else {
      val node = parseNode(input)

      // Get rest of unparsed data without the current "node" child
      val rest = input.slice(node.getSize, input.length)

      // Get rest of nodes
      val nodes = parseTree(rest, children - 1)

      node +: nodes
    }
  }


  def star1(): Int = {
    val input = Source.fromFile("src/main/scala/Day08/input08.txt").mkString("").split(" ").map(_.toInt)

    val root = parseTree(input, 1)(0)

    root.getMetadataSum
  }

  def star2(): Int = {
    val input = Source.fromFile("src/main/scala/Day08/input08.txt").mkString("").split(" ").map(_.toInt)

    val root = parseTree(input, 1)(0)

    root.getValue
  }
}
