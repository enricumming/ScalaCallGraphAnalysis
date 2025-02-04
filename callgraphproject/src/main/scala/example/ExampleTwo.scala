import scala.collection.mutable._

object ExampleTwo {
  def main(args: Array[String]): Unit = {
    val c = makeCollection(args(0))
    // Si es un ListBuffer, se puede llamar al metodo c.append para añadir un elemento
    // Si es un HashSet[String], se puede llamar al metodo c.add
    // Luego, += funciona para ambos , el cual es un llamado de método de tipo ApplyInfix
    c += "hello"
  }

  def makeCollection(s: String): Buffer[String] = {
    if (s == "list") {
      ListBuffer()
    } else {
      HashSet[String]().toBuffer
    }
  }
}

