package graph

class PropertyGraph {

  var G = Map[Node, Set[(Node, Relationship)]]()

  // Add a node to the property graph
  def addNode(n: Node): Unit =
    if (!G.contains(n))
      G += (n -> Set())

  // Add a relationship to the property graph
  def addRelationship(n: Node, m: Node, r: Relationship): Unit = {
    addNode(n)
    addNode(m)
    G += (n -> (G(n) + ((m, r))))
  }

  // Find nodes that match a name and/or a category
  // If the name isn’t given, ignore the name constraint
  // If the category isn’t given, ignore the category constraint
  def matchNode(name: String = "", category: String = ""): Set[Node] = {
    val returnAll = name == "" && category == ""
    val anyName = name == "" && category != ""
    val anyCat = name != "" && category == ""
    val specifyBoth = name != "" && category != ""

    var nodeSet = Set[Node]()
    for (n <- G.keys) {
      if (returnAll)
        nodeSet += n
      else if (anyName && n.category == category)
        nodeSet += n
      else if (anyCat && n.name == name)
        nodeSet += n
      else if (specifyBoth && n.name == name && n.category == category)
        nodeSet += n
    }
    nodeSet

  }

  // Find nodes adjacent to a given node
  // Optionally restrict the results to nodes matching a node category
  // and/or nodes connected via relationships involving a particular relationship category
  def adjacent(n: Node, nodeCategory: String = "", relCategory: String = ""): Set[Node] = {
    var adjNodes = Set[Node]()
    val currNodeAdjacents = G(n)
    for (n <- currNodeAdjacents) {
      val node:Node = n[0]
      val rel:Relationship = n[1]
      if (nodeCategory == "" && relCategory == "")
        adjNodes += node
      else if (relCategory == "" && node.category == nodeCategory)
        adjNodes += node
      else if (nodeCategory == "" && rel.category == relCategory)
        adjNodes += node
      else if (rel.category == relCategory && node.category == nodeCategory)
        adjNodes += node
    }
    adjNodes
  }

  // Find the subgraph consisting of the listed nodes and any relationships that occur
  // between anypair of nodes in the list.
  // Return a new PropertyGraph object
  def subGraph(nodes: List[Node]): PropertyGraph = {
    var pg = new PropertyGraph()


  }

  // String representation of the graph
  // Output something that is human-readable and that documents all of the nodes
  // and relationships and their properties
  override def toString: String = {
    var str = ""

    for (n <- G.keys)
      str += n.toString + "\t->\t" + G(n).toString() + "\n"
    str
  }
}
