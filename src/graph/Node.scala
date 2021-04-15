package graph

class Node(val name: String, val category: String) {

  var props = Map[String, Any]() // node properties

  // get a property from a node
  def getProperty(key: String): Any = props.getOrElse(key, None)

  // add a property to a node
  def addProperty(key: String, value: Any): Unit = props += (key->value)

  // return the node as a string representation
  override def toString:String = name + ':' + category
}
