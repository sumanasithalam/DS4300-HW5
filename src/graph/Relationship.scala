package graph

class Relationship(val category: String) {

  var props = Map[String, Any]() // node properties

  // Fetch a property from a relationship
  def getProperty(key: String): Any = props.getOrElse(key, None)

  // Add a property to a relationship
  def addProperty(key: String, value: Any): Unit = props += (key->value)

  // Returns a string representation of a Relationship object
  override def toString:String = category
}
