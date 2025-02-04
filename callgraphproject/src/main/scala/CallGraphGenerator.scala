import scala.meta.{Source => MetaSource, _}
import java.io.{File, PrintWriter}
import scala.io.{Source => IOSource}

object CallGraphGenerator {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Uso: CallGraphGenerator <archivo_input.scala> <archivo_output.dot>")
      System.exit(1)
    }

    val inputFilePath = args(0)
    val outputFilePath = args(1)

    // Leer el archivo de entrada
    val code = IOSource.fromFile(inputFilePath).mkString

    // Parsear el código fuente
    val source = code.parse[MetaSource].get

    // Almacenar llamadas como aristas
    val edges = scala.collection.mutable.ListBuffer[(String, String)]()

    // Obtener definiciones de métodos y sus llamadas, incluyendo el tipo
    source.collect {
      case obj: Defn.Object =>
        val objectName = obj.name.value // Nombre del objeto
        obj.collect {
          case defn: Defn.Def =>
            val caller = s"$objectName.${defn.name.value}" // Nodo con tipo
            defn.body.collect {
              case Term.Apply(Term.Name(callee), _) if callee != "println" =>
                val calleeWithType = s"$objectName.$callee" // Nodo con tipo
                edges += ((caller, calleeWithType))
            }
        }
      case cls: Defn.Class =>
        val className = cls.name.value // Nombre de la clase
        cls.collect {
          case defn: Defn.Def =>
            val caller = s"$className.${defn.name.value}" // Nodo con tipo
            defn.body.collect {
              case Term.Apply(Term.Name(callee), _) if callee != "println" =>
                val calleeWithType = s"$className.$callee" // Nodo con tipo
                edges += ((caller, calleeWithType))
            }
        }
    }

    // Crear el archivo .dot para Graphviz
    val graph = new StringBuilder("digraph CallGraph {\n")
    edges.foreach { case (caller, callee) =>
      graph.append(s"""  "$caller" -> "$callee";\n""")
    }
    graph.append("}")

    // Guardar el archivo .dot en el archivo de salida especificado
    val writer = new PrintWriter(new File(outputFilePath))
    writer.write(graph.toString())
    writer.close()

    println(s"Call graph generado en $outputFilePath")
  }
}

