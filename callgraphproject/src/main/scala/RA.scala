import scala.meta._
import scala.collection.mutable.Set
import scala.io.{Source => IOSource}
import java.io.{File, PrintWriter}

object RA {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Uso: ManualMethodCallDetector <archivo_input.scala> <archivo_output.dot>")
      System.exit(1)
    }

    val inputFilePath = args(0)
    val outputFilePath = args(1)

    // Leer el archivo de entrada
    val code = IOSource.fromFile(inputFilePath).mkString
    val source = code.parse[Source].get

    println("Estructura del AST del código:")
    println(source.structure)

    // Arcos almacenados
    val edges = Set[(String, String)]()

    // Llamadas de métodos detectadas
    println("\nLlamados de métodos encontrados:")
    traverseAST(source, edges, "main")

    // Mostrar los arcos
    println("\n Arcos del grafo:")
    edges.foreach {
      case (caller, callee) =>
        println(s"$caller -> $callee")
    }

    // Exportar el grafo en formato DOT
    exportToDot(edges, outputFilePath)
    println(s"Grafo exportado a $outputFilePath")
  }

  // Función para exportar a formato DOT
  def exportToDot(edges: Set[(String, String)], outputPath: String): Unit = {
    val dotContent = new StringBuilder("digraph CallGraph {\n")
    edges.foreach {
      case (caller, callee) =>
        val sanitizedCaller = caller.replace("\"", "\\\"") // Escapar comillas dobles
        val sanitizedCallee = callee.replace("\"", "\\\"") // Escapar comillas dobles
        dotContent.append(s"""  "$sanitizedCaller" -> "$sanitizedCallee";\n""")
    }
    dotContent.append("}\n")

    val writer = new PrintWriter(new File(outputPath))
    writer.write(dotContent.toString())
    writer.close()
  }

  // Recorrer el AST manualmente y detectar llamados de métodos
//  def traverseAST(tree: Tree, edges: Set[(String, String)], caller: String): Unit = {
//    tree match {
//      // Métodos directos
//      case Term.Apply(Term.Name(methodName), args) =>
//        if (methodName == "println") {
//          val argumentContent = args.map(_.syntax).mkString(", ")
//          val printlnNode = s"$methodName($argumentContent)"
//          println(s"Llamada a println diferenciada: $printlnNode")
//          edges += ((caller, printlnNode))
//        } else {
//          println(s"Método directo: $methodName con ${args.length} argumento(s)")
//          edges += ((caller, methodName))
//        }
//        args.foreach(arg => traverseAST(arg, edges, methodName))
//
//      // Métodos infijos
//      case Term.ApplyInfix(lhs, Term.Name(methodName), _, args) =>
//        if (!Set("args", "==", "!=", "<", ">", "<=", ">=").contains(methodName)) {
//          println(s"Método infijo: $methodName con ${args.length} argumento(s)")
//          edges += ((caller, methodName))
//          traverseAST(lhs, edges, caller)
//          args.foreach(arg => traverseAST(arg, edges, caller))
//        }
//
//      // Métodos seleccionados
//      case Term.Select(qual, Term.Name(methodName)) =>
//        val context = qual match {
//          case Term.ApplyType(Term.Name(className), _) =>
//            s"$className.<init>"
//          case _ =>
//            qual.syntax
//        }
//        val compositeNode = s"$context.$methodName"
//        println(s"Método seleccionado compuesto: $compositeNode")
//        edges += ((caller, compositeNode))
//
//
//      // Constructores genéricos
//      case Term.ApplyType(Term.Name(className), _) =>
//        val constructorNode = s"$className.<init>"
//        println(s"Constructor genérico detectado: $constructorNode")
//        edges += ((caller, constructorNode))
//
//      // Constructores estándar
//      case Term.Apply(Term.Name(className), args) =>
//        val constructorNode = s"$className.<init>"
//        println(s"Constructor estándar detectado: $constructorNode con ${args.length} argumento(s)")
//        edges += ((caller, constructorNode))
//        args.foreach(arg => traverseAST(arg, edges, constructorNode))
//
//      // Constructores explícitos
//      case Term.New(init) =>
//        val className = init.tpe.toString
//        val constructorNode = s"$className.<init>"
//        println(s"Constructor detectado: $constructorNode")
//        edges += ((caller, constructorNode))
//        traverseAST(init, edges, constructorNode)
//
//      // Definiciones de métodos
//      case defn: Defn.Def =>
//        val newCaller = defn.name.value
//        println(s"Entrando en el método: $newCaller")
//        defn.body.children.foreach(child => traverseAST(child, edges, newCaller))
//
//      // Estructuras If
//      case Term.If(cond, thenp, elsep) =>
//        println(s"Procesando If con condición: ${cond.syntax}")
//        traverseAST(cond, edges, caller)
//        traverseAST(thenp, edges, caller)
//        traverseAST(elsep, edges, caller)
//
//          // Ignorar nodos irrelevantes
//          case _: Import | _: Importer | _: Importee =>
//
//          // Caso por defecto
//          case _ =>
//            tree.children.foreach(child => traverseAST(child, edges, caller))
//        }
//    }
// Conjunto para almacenar definiciones de métodos y clases
val methodDefinitions = Set[String]()

  // Modificar `traverseAST`
  def traverseAST(tree: Tree, edges: Set[(String, String)], caller: String): Unit = {
    tree match {
      // Definiciones de métodos
      case defn: Defn.Def =>
        val methodName = defn.name.value
        println(s"Entrando en el método: $methodName")
        traverseAST(defn.body, edges, methodName) // Recorrer el cuerpo completo del método

      // Métodos directos
      case Term.Apply(Term.Name(methodName), args) =>
        if (methodName == "println") {
          val argumentContent = args.map(_.syntax).mkString(", ")
          val printlnNode = s"""$methodName("$argumentContent")"""
          println(s"Llamada a println diferenciada: $caller -> $printlnNode")
          edges += ((caller, printlnNode)) // Registrar arco hacia println
        } else {
          println(s"Llamado directo detectado: $caller -> $methodName")
          edges += ((caller, methodName))
        }
        args.foreach(arg => traverseAST(arg, edges, methodName))

      // Métodos seleccionados (e.g., obj.method())
      case Term.Select(_, Term.Name(methodName)) =>
        println(s"Llamado seleccionado: $caller -> $methodName")
        edges += ((caller, methodName))

      // Constructores explícitos
      case Term.New(init) =>
        val className = init.tpe.toString
        val constructorNode = s"$className.<init>"
        println(s"Constructor detectado: $caller -> $constructorNode")
        edges += ((caller, constructorNode))

      // Otros casos
      case _ =>
        tree.children.foreach(child => traverseAST(child, edges, caller)) // Recorrer todos los hijos
    }
  }


}






