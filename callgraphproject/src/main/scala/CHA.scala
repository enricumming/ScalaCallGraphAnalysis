import scala.meta._
import scala.collection.mutable.{Set, Map,Queue}
import scala.io.{Source => IOSource}
import java.io.PrintWriter
import java.io.File
import scala.util.matching.Regex

object CHA {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Uso: runMain CHA <directorio_proyecto> <archivo_output.dot>")
      System.exit(1)
    }

    //Se toman dos parametros: El archivo Scala que contiene codigo fuente y archivo de salida .dot que genera el grafo.
    val inputDirectory = args(0)
    val outputFilePath = args(1)
    val scalaFiles = getScalaFiles(inputDirectory)

    // Generar una lista de árboles AST a partir de los archivos Scala
    val sourceTrees = scalaFiles.flatMap { file =>
      val code = IOSource.fromFile(file).mkString.trim
      if (code.isEmpty) {
        println(s"️ Archivo vacío omitido: ${file.getName}")
        None
      } else {
        try {
          Some(code.parse[Source].get)
        } catch {
          case e: Exception =>
            println(s" Error al parsear ${file.getName}: ${e.getMessage}")
            None
        }
      }
    }

    /**
     *  Mapas para jerarquía de clases, tipos de variables e implementacion de metodos.
     *  classHierarchy: Almacena la jerarquia de clases tal que: Superclase -> subclases
     *  variableTypes: Almacena los tipos de variables. Ej: dog -> Animal (variable creada dog es de tipo Animal)
     *  methodImplementations: Almacena las clases que implementan un método específico. Ej: makeSound -> Dog, Akita, Cat, Robot
     *  Sets
     *  graphNodes: Conjunto de nodos del grafo.
     *  graphEdges: Conjunto de arcos del grafo (caller -> callee).
     *  unreachableNodes: Nodos que no tienen conexiones y se consideran inalcanzables.
     */
    val classHierarchy = Map[String, Set[String]]()
    val variableTypes = Map[String, String]()
    val methodImplementations = Map[String, Set[String]]()
    val graphNodes = scala.collection.mutable.Set[String]()

    val graphEdges = scala.collection.mutable.Set[(String, String)]()
    val unreachableNodes = Set[String]()
    val routesFilePath = s"$inputDirectory/conf/routes"
    val entryPoints = extractEntryPointsFromRoutes(routesFilePath)

    println(s"Los entry points del programa son : $entryPoints")

    // Normalizar los nombres de los entry points para asegurar consistencia con el grafo
//    val normalizedEntryPoints = entryPoints.map(normalizeNodeName)

    // Agregarlos como nodos iniciales en el grafo
    graphNodes ++= entryPoints

//     Analizar cada archivo
    sourceTrees.foreach { source =>
      buildClassHierarchy(source, classHierarchy, methodImplementations)
      generateGraphFromAST(source, graphNodes, graphEdges, classHierarchy, variableTypes)
    }
    println(s"Jerarquia de clases es: $classHierarchy")
    println(s"Implementacion de metodos son: $methodImplementations")
    println(s" Tipos de las variables: $variableTypes")

    // Buscar nuevos métodos llamados desde los nodos finales del grafo en todos los archivos
    findCalleesFromGraphNodes(graphNodes, graphEdges, sourceTrees, variableTypes)

    // Aplicar CHA
    applyCHA(graphNodes, graphEdges, classHierarchy, methodImplementations, sourceTrees, variableTypes)

    //  Normalizar nodos y arcos
    normalizeNodes(graphNodes, graphEdges, unreachableNodes)

    // Marcar nodos inalcanzables
    sourceTrees.foreach { source =>
      addUnreachableNodes(source, graphNodes, methodImplementations, graphEdges, unreachableNodes)
      processUnreachableNodes(unreachableNodes, graphEdges, source)
      propagateUnreachability(graphEdges, unreachableNodes)
    }

     // Normalizar nodos y arcos
    normalizeNodes(graphNodes, graphEdges, unreachableNodes)

    // Exportar a archivo .dot
    exportGraphToDot(graphNodes, graphEdges, unreachableNodes, outputFilePath)

    println(s"\nGrafo exportado a '$outputFilePath'")

    println(s"Los nodos encontrados son : $graphNodes")

    // Generar .csv para identificar nodos alcanzables e inalcanzables
    val writer = new PrintWriter(s"$outputFilePath.csv")

    // Escribir encabezado
    writer.println("Metodo,Estado")

    // Agregar métodos alcanzables
    graphNodes.foreach { node =>
      writer.println(s"$node, Alcanzable")
    }

    // Agregar métodos inalcanzables
    unreachableNodes.foreach { node =>
      writer.println(s"$node, Inalcanzable")
    }

    writer.close()
    println(s"\n Archivo generado: $outputFilePath.csv")

  }

  /**
   * Extrae los métodos de los controladores desde el archivo conf/routes en un proyecto Play Framework.
   *
   * @param routesFilePath Ruta del archivo conf/routes
   * @return Un conjunto con los métodos de los controladores como "UserController.getUsers"
   */
def extractEntryPointsFromRoutes(routesFilePath: String): Set[String] = {
  val entryPoints = scala.collection.mutable.Set[String]()
//  val paramPattern: Regex = new Regex("\\(([^)]*)\\)") // Expresión regular para capturar parámetros
  val paramPattern: Regex = new Regex("\\((.*)\\)")
  try {
    val source = IOSource.fromFile(routesFilePath)
    for (line <- source.getLines()) {
      val parts = line.split("\\s+").filter(_.nonEmpty) // Separar por espacios
      if (parts.length >= 3) {
        val controllerMethod = parts(2) // La tercera parte es "controllers.UserController.getUsers"
        val methodParts = controllerMethod.split("\\.") // Separar en controlador y método
        if (methodParts.length >= 2) {
          // Extraer el nombre del método y sus parámetros (si existen)
          val controller = methodParts.init.mkString(".") // "controllers.UserController"
          val methodWithParams = methodParts.last // "getUsers" o "show(id: Int)"

          // Eliminar el prefijo "controllers." para normalizar los nombres
          val normalizedController = controller.stripPrefix("controllers.")

          // Normalizar los parámetros: conservar su tipo si existe
          val formattedMethod = paramPattern.replaceAllIn(methodWithParams, m => {
            val params = m.group(1).split(",").map(_.trim).filter(_.nonEmpty).map(param => {
                val parts = param.split(":").map(_.trim)
                if (parts.length == 2) s"${parts(0)}: ${parts(1)}"// Asegurar "nombre: tipo"
                else parts(0).stripSuffix(":") // Elimina ":" finales si no hay tipo
              }).mkString(", ")
            if (params.nonEmpty) s"($params)" else "()"
          })

          entryPoints += s"$normalizedController.$formattedMethod"
        }
      }
    }
    source.close()
  } catch {
    case e: Exception => println(s"Error al leer el archivo conf/routes: ${e.getMessage}")
  }

  entryPoints
}

  /**
   * Recorre el AST buscando definiciones de clases y traits y construye las variables classHierarchy y methodImplementations.
   *
   * @param tree: AST generado por Scalameta
   * @param classHierarchy: Mapeo que almacena la jerarquia de clases tal que: Superclase -> subclases
   * @param methodImplementations: Mapeo que almacena las clases que implementan un método específico.
   */
  def buildClassHierarchy(
                           tree: Tree,
                           classHierarchy: Map[String, Set[String]],
                           methodImplementations: Map[String, Set[String]]
                         ): Unit = {
    /**
     * tree.collect: Recorre el AST en busca de nodos por ejemplo, Defn.Class
     * Defn.class : Nodo en el cual se define una clase. Ej: class Dog extends Animal
     * defn.name.value = extraer el nombre del nodo de la clase. Ej: Dog
     * defn.templ  : Cada definicion de clase contiene una propiedad templ (template). Contiene la estructura interna de una clase
     * Representa:  Las superclases o supertraits que la clase extiende.
                    Los métodos y valores definidos dentro de la clase o trait.
                    Las inicializaciones y otros detalles internos.
     * inits : Dentro de templ, exista una lista llamada inits. Esta contiene las clases o traits base que la clase esta extendiendo o implementando.
     *        Cada elemento de esta lista es un objeto de tipo Init, que representa una clase base o trait base y su constructor llamado (si lo hay).
     * init.tpe.syntax: Entrega el nombre de la clase padre
     * stats : Dentro de templ, stats es una lista que contiene todas las definiciones y expresiones dentro de la clase, trait u objeto. Por ej:
     *        Defn.Def = Metodo.
     *        Defn.Val = Valor
     *        Defn.Var = Variable
     *        Defn.Class = Clase interna
     *
     */
    tree.collect {
      case defn: Defn.Class =>
        val className = defn.name.value
        defn.templ.inits.foreach { init =>
          val parentName = init.tpe.syntax
          // Se actualiza el mapa de la jerarquia de clases
          classHierarchy.getOrElseUpdate(parentName, Set()) += className
        }
        defn.templ.stats.foreach {
          // Captura los metodos dentro de la clase encontrada en el AST. Luego, se actualiza el mapeo de implementacion de metodos.
          case method: Defn.Def =>
            val methodName = method.name.value
            methodImplementations.getOrElseUpdate(methodName, Set()) += className
          case _ =>
        }
      // Analogo para Traits.
      case defn: Defn.Trait =>
        val traitName = defn.name.value
        defn.templ.inits.foreach { init =>
          val parentName = init.tpe.syntax
          classHierarchy.getOrElseUpdate(parentName, Set()) += traitName
        }
        defn.templ.stats.foreach {
          case method: Defn.Def =>
            val methodName = method.name.value
            methodImplementations.getOrElseUpdate(methodName, Set()) += traitName
          case _ =>
        }
    }
  }

  /**
   * Recorre un AST desde el punto de entrada main() y genera los primeros nodos y arcos de un Call graph en base a un codigo fuente.
   *
   * @param tree: AST generado por Scalameta
   * @param graphNodes: Set de nodos que representan llamados de metodo de un codigo fuente
   * @param graphEdges: Set de arcos que une nodos tal que se genere un Call Graph
   * @param classHierarchy: Mapeo que almacena la jerarquia de clases tal que: Superclase -> subclases
   * @param variableTypes: Mapeo que permite conocer el tipo de las variables inicializadas en el codigo fuente
   */
  def generateGraphFromAST(
                            tree: Tree,
                            graphNodes: scala.collection.mutable.Set[String],
                            graphEdges: scala.collection.mutable.Set[(String, String)],
                            classHierarchy: Map[String, Set[String]],
                            variableTypes: Map[String, String]
                          ): Unit = {

    /**
     * Agrega un arco al call graph, siendo este una representacion de llamadas de metodos entre dos nodos.
     *
     * @param caller: Nodo que llama a un metodo
     * @param callee: Nodo que es llamado
     */
    def addEdge(caller: String, callee: String): Unit = {
      if (!graphEdges.contains((caller, callee))) {
        graphEdges += (caller -> callee)
        println(s"Agregando arco: $caller -> $callee")
      }
    }

    /**
     * Recorre el AST desde el nodo main e identifica llamadas de metodos, nuevas instancias de clases y bloques de codigos.
     *
     * @param currentNode: Nodo actual desde el cual se comienza a hacer un analisis del flujo de un codigo fuente.
     * @param caller: Nodo que llama a un metodo
     */
    def processNode(currentNode: Tree, caller: String): Unit = {
      currentNode match {
        // Detecta si el nodo es la definicion del metodo main y comienza a procesarlo como nodo inicial del grafo
        case defn: Defn.Def if defn.name.value == "main" =>
          //Busca el objeto,clase o trait que contiene el metodo main.
          val enclosingObject = findEnclosingClassOrTrait(defn, tree)
          val mainCaller = s"$enclosingObject.main"
          graphNodes += mainCaller
          println(s"Nodo '$mainCaller' encontrado, comenzando el recorrido.")
          // Recorre el cuerpo del metodo main
          processNode(defn.body, mainCaller)

        case defn: Defn.Def =>
          println(s"Procesando definición de método: ${defn.name.value}")
          val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
          // Agregar parámetros del método al mapeo variableTypes
          defn.paramss.flatten.foreach { param =>
            (param.name, param.decltpe) match {
              case (name: Name, Some(paramType)) =>
                variableTypes(name.value) = paramType.syntax
                println(s"Inferido: ${name.value} -> ${paramType.syntax}")
              case _ =>
            }
          }

          // Procesar el cuerpo del método como de costumbre
          processNode(defn.body, s"$enclosingClassOrTrait.${defn.name.value}")

        //Detecta llamadas de metodos de un objeto. Ej: dog.makeSound()
        // Agrega las llamadas encontradas como un nodo junto al objeto que hace el llamado. Luego, agrega un arco desde el nodo
        // caller al nodo encontrado.

        case Term.Apply(Term.Select(obj, Term.Name(methodName)), args) =>
          //Obtiene el contexto u objeto que hace la llamada.
          val contextType = resolveFullQualifiedName(obj, variableTypes)

          val methodWithType = if (args.nonEmpty) {
            s"$contextType.$methodName(${args.map(_.syntax).mkString(", ")})"
          } else {
            s"$contextType.$methodName()"
          }

          graphNodes += methodWithType
          addEdge(caller, methodWithType)

          //Recorre los argumentos de las llamadas a metodos y se procesan recursivamente. Permite recorrer el flujo del codigo fuente.
          // Permite encontrar llamadas a metodos anidadas, flujos indirectos, etc.
          args.foreach(arg => processNode(arg, methodWithType))

        // Detecta llamadas de metodos directas junto a su argumento. Ej: println("...")
        // Se agrega al Set de nodos, y luego se agrega un arco correspondiente.
        case Term.Apply(Term.Name(methodName), args) =>
          val detailedMethodCall = s"$methodName(${args.map(_.syntax).mkString(", ")})"
          graphNodes += detailedMethodCall
          addEdge(caller, detailedMethodCall)

        // Detecta nuevas instancias de clases. Ej: new Dog() y lo agrega como un nodo y arco correspondiente.
        case Term.New(init) =>
          val constructorCall = s"${init.tpe.syntax}.<init>"
          graphNodes += constructorCall
          addEdge(caller, constructorCall)

        // Procesa un bloque de codigo con multiples instrucciones en busca de algun llamado de metodo.
        case Term.Block(stats) =>
          stats.foreach(stat => processNode(stat, caller))

        case Defn.Val(_, List(Pat.Var(Term.Name(varName))), typeOpt, init) =>
          print(s"Entre aca init: $init. varName: $varName")

          val inferredType = typeOpt match {
            case Term.Apply(Term.Name("List"), args) => s"List[${inferTypeFromArgs(args)}]"
            case Term.Apply(Term.Name("Seq"), args)  => s"Seq[${inferTypeFromArgs(args)}]"
            case Some(Type.Name(typeName)) => typeName // Tipo explícito
            case Some(other) =>
              println(s"️ Tipo no reconocido en typeOpt: $other")
              other.toString // Imprime el tipo desconocido y lo usa como string
            case None =>
              // Inferir tipo si es una inicialización con `new`
              init match {
                case Term.New(initExpr) => initExpr.tpe.syntax
                case Lit.String(_)      => "String"
                case Lit.Int(_)         => "Int"
                case Lit.Double(_)      => "Double"
                case Lit.Boolean(_)     => "Boolean"
                case _                  => "" // Si no se puede inferir
              }
          }
          println(s"Inferido 2: $varName -> $inferredType")
          variableTypes(varName) = inferredType
          processNode(init, caller)

        //Si el currentNode no coincide con ningun patron anterior, recorre los nodos hijos del nodo actual y los procesa.
        case _ =>
          currentNode.children.foreach(child => processNode(child, caller))
      }
    }
    // Cuando encuentra el metodo main en el AST lo procesa como un nodo inicial.
//    tree.collect {
//      case defn: Defn.Def if defn.name.value == "main" =>
//        processNode(defn, "main")
//    }

    // 🔹 Procesar todas las clases, objetos y traits como entry points del AST
    tree.collect {
      case cls: Defn.Class =>
        println(s" Procesando clase: ${cls.name.value}")
        cls.templ.stats.foreach(stat => processNode(stat, cls.name.value))

      case obj: Defn.Object =>
        println(s"Procesando objeto: ${obj.name.value}")
        obj.templ.stats.foreach(stat => processNode(stat, obj.name.value))

      case trt: Defn.Trait =>
        println(s" Procesando trait: ${trt.name.value}")
        trt.templ.stats.foreach(stat => processNode(stat, trt.name.value))
    }

    // 🔹 Procesar entry points extraídos del archivo routes
    graphNodes.foreach { entryPoint =>
      tree.collect {
        case defn: Defn.Def if normalizeNodeName(s"${findEnclosingClassOrTrait(defn, tree)}.${defn.name.value}") == entryPoint =>
          println(s" Entry point encontrado en AST: $entryPoint")
          processNode(defn, entryPoint)
      }
    }
    println(s" Estado final de variableTypes: $variableTypes")
  }

  /**
   * Funcion aux
   * @param args
   * @return
   */
  def inferTypeFromArgs(args: List[Term]): String = {
    args.headOption match {
      case Some(Lit.String(_))  => "String"
      case Some(Lit.Int(_))     => "Int"
      case Some(Lit.Double(_))  => "Double"
      case Some(Lit.Boolean(_)) => "Boolean"
      case _                    => ""
    }
  }

  /**
   *
   * @param obj
   * @param variableTypes
   * @return
   */
  def resolveFullQualifiedName(obj: Term, variableTypes: Map[String, String]): String = {
    obj match {
      case Term.Select(qualifier, Term.Name(name)) =>
      name
      case Term.Name(name) =>
        variableTypes.getOrElse(name, {
          if (name.matches("\".*\"")) "String" // Si es una cadena literal
          else name
        })
      case _ =>
        ""
    }
  }

  /**
   * Implementa la logica del algoritmo CHA para resolver la creacion de nodos en caso de llamados polimorficos en el codigo fuente.
   * Busca subtipos en la jerarquia de clases, detecta implementaciones de metodos con el mismo nombre que las clases padres y luego se agregan nodos/arcos correspondientes.
   *
   * @param graphNodes: Conjunto de nodos del grafo.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param classHierarchy: Mapeo que almacena la jerarquia de clases tal que: Superclase -> subclases
   * @param methodImplementations: Mapeo que almacena las clases que implementan un método específico.
   * @param source: AST generado por Scalameta
   */
  def applyCHA(
                graphNodes: scala.collection.mutable.Set[String],
                graphEdges: scala.collection.mutable.Set[(String, String)],
                classHierarchy: Map[String, Set[String]],
                methodImplementations: Map[String, Set[String]],
                sourceTrees: List[Tree],
                variableTypes: Map[String, String]
              ): Unit = {
    val newEdges = Set[(String, String)]()

    graphEdges.foreach { case (caller, callee) =>
      callee.split("\\.") match {
        case Array(contextType, methodName) =>
          val normalizedMethodName = methodName.replaceAll("\\(.*\\)", "") // Elimina paréntesis
          println(s"El metodo normalizado a analizar: $normalizedMethodName, y su contexto: $contextType")
          //  Buscar todas las subclases y superclases
          val relatedClasses = findAllSubtypes(contextType, classHierarchy) ++ findAllSupertypes(contextType, classHierarchy)
          println(s"La clase encontrada es: $relatedClasses")
          //  Filtrar solo clases que implementan el método
          val resolvedTargets = relatedClasses.filter { cls =>
            methodImplementations.getOrElse(normalizedMethodName, Set()).contains(cls)
          }

          //  Agregar nodos y arcos para cada implementación válida
          resolvedTargets.foreach { targetClass =>
            val targetMethod = s"$targetClass.$methodName"
            if (!graphEdges.contains((caller, targetMethod))) {
              graphNodes += targetMethod
              newEdges += (caller -> targetMethod)
              println(s" Agregando método de subtipo o supertipo con CHA: $caller -> $targetMethod")

              //  Revisar el cuerpo del método recién agregado
              findCalleesFromGraphNodes(graphNodes, newEdges, sourceTrees, variableTypes)
            }
          }
        case _ =>
          println(s"Advertencia: formato inesperado en callee '$callee'")
      }
    }
    // Se actualiza el conjunto de arcos con los nuevos descubiertos
    graphEdges ++= newEdges
  }

  /**
   * Encuentra todos los subtipos asociados a un tipo de acuerdo a la jerarquia de clases construida en base al codigo fuente.
   *
   * @param baseType: Clase o tipo de un objeto.
   * @param classHierarchy: Mapeo que almacena la jerarquia de clases tal que: Superclase -> subclases
   * @return: Set que contiene los subtipos asociados a una clase.
   */
  def findAllSubtypes(baseType: String, classHierarchy: Map[String, Set[String]]): Set[String] = {
    val directSubtypes = classHierarchy.getOrElse(baseType, Set())
    val allSubtypes = Set[String]()

    def recurse(currentType: String): Unit = {
      if (!allSubtypes.contains(currentType)) {
        allSubtypes += currentType
        classHierarchy.getOrElse(currentType, Set()).foreach(recurse)
      }
    }
    directSubtypes.foreach(recurse)
    allSubtypes
  }

  /**
   * Encuentra todas las clases padres asociados a un tipo de acuerdo a la jerarquía de clases.
   *
   * @param baseType: Clase o tipo de un objeto.
   * @param classHierarchy: Mapeo que almacena la jerarquía de clases (Superclase -> Subclases).
   * @return: Set que contiene las superclases asociadas a una clase.
   */
  def findAllSupertypes(baseType: String, classHierarchy: Map[String, Set[String]]): Set[String] = {
    val parents = Set[String]()
    //  Llenamos `parents` con las superclases directas de `baseType`
    classHierarchy.foreach { case (parent, subclasses) =>
      if (subclasses.contains(baseType)) {
        parents += parent
      }
    }
    //  Usamos `foreach` en lugar de `++` para mantener mutabilidad
    parents.foreach { p =>
      parents ++= findAllSupertypes(p, classHierarchy)
    }
    parents // Retornamos el `mutable.Set`
  }


  /**
   * Recorre el AST en busca de llamados de metodos  desde los nodos finales del grafo.
   *
   * @param graphNodes: Conjunto de nodos del grafo.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param tree: AST generado por Scalameta
   * @param variableTypes: Mapeo que permite conocer el tipo de las variables inicializadas en el codigo fuente
   */

//def findCalleesFromGraphNodes(
//                               graphNodes: scala.collection.mutable.Set[String],
//                               graphEdges: scala.collection.mutable.Set[(String, String)],
//                               sourceTrees: List[Tree], // Recorre todos los ASTs
//                               variableTypes: Map[String, String],
//                             ): Unit = {
//  val pendingNodes = scala.collection.mutable.Queue(graphNodes.toSeq: _*)
//  println(s"Los nodos pendientes son: $pendingNodes")
//  while (pendingNodes.nonEmpty) {
//    val currentNode = pendingNodes.dequeue()
//    println(s"Procesando nodo: $currentNode")
//
//    // Buscar definición del nodo en el AST
//    sourceTrees.foreach { tree =>
//    tree.collect {
//      case defn: Defn.Def =>
//        val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
//        val paramTypes = defn.paramss.flatten.map(_.decltpe.map(_.syntax).getOrElse("_")).mkString(", ")
//        val expectedNode = buildNodeName(enclosingClassOrTrait, defn.name.value, paramTypes)
//
//        // Modificar el nodo actual para deducir tipos
//        val currentNodeModified = modifyNodeWithParameterTypes(currentNode, variableTypes)
//        println(s"Comparando nodo: $currentNodeModified con esperado: $expectedNode")
//
//        if (currentNodeModified == expectedNode) {
//          println(s"Revisando cuerpo de $currentNode para nuevos métodos.")
//
//          // **Agregar parámetros al mapeo variableTypes**
//          defn.paramss.flatten.foreach { param =>
//            (param.name, param.decltpe) match {
//              case (name: Name, Some(paramType)) =>
//                variableTypes(name.value) = paramType.syntax
//                println(s"Tipo variable inferido: ${name.value} -> ${paramType.syntax}")
//              case _ =>
//            }
//          }
//          // Recorrer el cuerpo del método y buscar métodos llamados
//          processMethodBody(defn.body, currentNode, enclosingClassOrTrait, graphNodes, graphEdges, pendingNodes, variableTypes)
//        }
//    }
//    }
//  }
//  println(s"Análisis completo. Nodos: ${graphNodes.mkString(", ")}")
//}

  def findCalleesFromGraphNodes(
                                 graphNodes: Set[String],
                                 graphEdges: Set[(String, String)],
                                 sourceTrees: List[Tree],
                                 variableTypes: Map[String, String]
                               ): Unit = {

    val pendingNodes = Queue(graphNodes.toSeq: _*)
    println(s"Los nodos pendientes son: $pendingNodes")

    // **Paso 1: Construir un índice de métodos para búsqueda rápida**
    val methodIndex: Map[String, Defn.Def] = Map(
      sourceTrees.flatMap { tree =>
        tree.collect {
          case defn: Defn.Def =>
            val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
            val paramTypes = defn.paramss.flatten.flatMap(_.decltpe.map(_.syntax)).mkString(", ")
            val methodName = buildNodeName(enclosingClassOrTrait, defn.name.value, paramTypes)
            methodName -> defn
        }
      }: _*
    )

    println(s"Índice de métodos construido: ${methodIndex.keys.mkString(", ")}")

    // **Paso 2: Procesar nodos pendientes**
    while (pendingNodes.nonEmpty) {
      val currentNode = pendingNodes.dequeue()
      println(s"Procesando nodo: $currentNode")

      // **Buscar el nodo directamente en el índice**
      val currentNodeModified = modifyNodeWithParameterTypes(currentNode, variableTypes)
      methodIndex.get(currentNodeModified) match {
        case Some(defn) =>
          println(s"Encontrado en índice: Revisando cuerpo de $currentNode para nuevos métodos.")

          // **Agregar parámetros al mapeo variableTypes**
          defn.paramss.flatten.foreach { param =>
            param.decltpe match {
              case Some(paramType) =>
                variableTypes(param.name.value) = paramType.syntax
                println(s"Tipo variable inferido: ${param.name.value} -> ${paramType.syntax}")
              case None =>
            }
          }

          // **Procesar el cuerpo del método**
          processMethodBody(
            defn.body,
            currentNode,
            findEnclosingClassOrTrait(defn, sourceTrees.head),
            graphNodes, graphEdges, pendingNodes, variableTypes
          )

        case None =>
          println(s"No se encontró definición de $currentNode en el índice")
      }
    }

    println(s"Análisis completo. Nodos: ${graphNodes.mkString(", ")}")
  }



  /**
   *
   * @param body
   * @param currentNode
   * @param enclosingClassOrTrait
   * @param graphNodes
   * @param graphEdges
   * @param pendingNodes
   * @param variableTypes
   */
  def processMethodBody(
                         body: Term,
                         currentNode: String,
                         enclosingClassOrTrait: String,
                         graphNodes: scala.collection.mutable.Set[String],
                         graphEdges: scala.collection.mutable.Set[(String, String)],
                         pendingNodes: scala.collection.mutable.Queue[String],
                         variableTypes: Map[String, String]
                       ): Unit = {
    println(s"Nodo que se analiza: $currentNode")

    body.collect {
      case methodCall: Term =>
        extractMethodCall(methodCall, currentNode, enclosingClassOrTrait, graphNodes, graphEdges, pendingNodes, variableTypes)
    }
  }

  /**
   *
   * @param term
   * @param caller
   * @param enclosingClassOrTrait
   * @param graphNodes
   * @param graphEdges
   * @param pendingNodes
   * @param variableTypes
   */
  def extractMethodCall(
                         term: Term,
                         caller: String,
                         enclosingClassOrTrait: String,
                         graphNodes: scala.collection.mutable.Set[String],
                         graphEdges: scala.collection.mutable.Set[(String, String)],
                         pendingNodes: scala.collection.mutable.Queue[String],
                         variableTypes: Map[String, String]
                       ): Unit = {
    term match {
      // Detectar llamadas a métodos como `foo()`
      case Term.Apply(Term.Name(methodName), args) =>
        val newNode = buildNodeName(enclosingClassOrTrait, methodName, args.map(_.syntax).mkString(", "))
        println(s"Detectada llamada a método directo: $newNode")
        addNodeAndEdge(caller, newNode, graphNodes, graphEdges, pendingNodes)

      // Detectar llamadas de métodos en objetos como `obj.method()`
      case Term.Apply(Term.Select(obj, Term.Name(methodName)), args) =>
        val contextType = resolveFullQualifiedName(obj, variableTypes)
        val newNode = buildNodeName(contextType, methodName, args.map(_.syntax).mkString(", "))
        println(s"Detectada llamada a método con objeto: $newNode")
        addNodeAndEdge(caller, newNode, graphNodes, graphEdges, pendingNodes)

      // Detectar referencias a métodos como `obj.method` sin `Apply`
      case Term.Select(obj, Term.Name(methodName)) =>

        val contextType = resolveFullQualifiedName(obj, variableTypes)
        val newNode = s"$contextType.$methodName()"
        println(s"Detectada referencia a método: $newNode")
        addNodeAndEdge(caller, newNode, graphNodes, graphEdges, pendingNodes)

      // Si el nodo tiene hijos, analizarlos recursivamente
      case other =>
        other.children.collect { case t: Term => t }
          .foreach(child => extractMethodCall(child, caller, enclosingClassOrTrait, graphNodes, graphEdges, pendingNodes, variableTypes))

    }
  }

  /**
   *
   * @param caller
   * @param callee
   * @param graphNodes
   * @param graphEdges
   * @param pendingNodes
   */
  def addNodeAndEdge(
                      caller: String,
                      callee: String,
                      graphNodes: Set[String],
                      graphEdges: Set[(String, String)],
                      pendingNodes: Queue[String]
                    ): Unit = {
    if (!graphEdges.contains((caller, callee))) {
      graphEdges += ((caller, callee))
      println(s"Agregando arco: $caller -> $callee")
    }
    if (!graphNodes.contains(callee)) {
      graphNodes += callee
      pendingNodes.enqueue(callee)
      println(s"Agregando nodo: $callee a la cola de procesamiento.")
    }
  }

  /**
   * Funcion auxiliar que identifica clase, objeto o trait que contiene un metodo y lo retorna como String
   *
   * @param defn: Definicion de un metodo encontrada en codigo fuente
   * @param tree: AST generado por Scalameta
   * @return: Clase, objeto o trait identificado
   */
  def findEnclosingClassOrTrait(defn: Defn.Def, tree: Tree): String = {
    def findEnclosing(node: Tree): Option[String] = {
      node match {
        case obj: Defn.Object if obj.templ.stats.contains(defn) => Some(obj.name.value)
        case cls: Defn.Class if cls.templ.stats.contains(defn) => Some(cls.name.value)
        case trt: Defn.Trait if trt.templ.stats.contains(defn) => Some(trt.name.value)
        case _ => node.children.flatMap(findEnclosing).headOption
      }
    }
    findEnclosing(tree).getOrElse("")

  }
  /**
   *
   * @param context
   * @param name
   * @param paramTypes
   * @return
   */
  def buildNodeName(context: String, name: String, paramTypes: String): String = {
    if (paramTypes.isEmpty) s"$context.$name()" else s"$context.$name($paramTypes)"
  }

  /**
   *
   * @param node
   * @return
   */

  def modifyNodeWithParameterTypes(node: String, variableTypes: Map[String, String]): String = {
    val cleanedNode = node.replaceAll("\\[.*?\\]", "") // Elimina restricciones de tipo como `[B]`

    if (cleanedNode.contains("(") && cleanedNode.contains(")")) {
      val rawParams = cleanedNode.substring(
        cleanedNode.indexOf("(") + 1,
        cleanedNode.lastIndexOf(")")
      )

      // Extraer los parámetros correctamente
      val paramValues = extractParameterValues(rawParams)

      val paramTypes = paramValues.map { value =>
        variableTypes.getOrElse(value, {
          value match {
            case v if v.matches("\\d+")       => "Int"
            case v if v.matches("\\d+\\.\\d+") => "Double"
            case v if v.matches("\".*\"")      => "String"
            case v if v.startsWith("s\"")      => "String"
            case _                             => ""
          }
        })
      }.filter(_ != "").mkString(", ")

      val methodName = cleanedNode.substring(0, cleanedNode.indexOf("("))
      if (paramTypes.isEmpty) s"$methodName()" else s"$methodName($paramTypes)"
    } else {
      cleanedNode
    }
  }


  /**
   * Funcion auxiliar para extraer los parametros del llamado de un metodo y retornar una lista con ellos
   *
   * @param params
   * @return Lista con los parametros de una funcion
   */
  def extractParameterValues(params: String): List[String] = {
  params.split(",").map(_.trim).toList
}

  /**
   *
   * @param node
   * @return
   */
  def normalizeNodeName(node: String): String = {
    //Si es solo para comparación, eliminamos los `[T]`, `[B]`, etc.
    val processedNode =  node.replaceAll("\\[.*?\\]", "")

    if (processedNode.contains("(")) {
      val base = processedNode.substring(0, processedNode.indexOf("("))
      if (processedNode.endsWith("()")) base else s"$base()"
    } else {
      processedNode
    }
  }

  /**
   * Recorre el AST para encontrar todos los metodos definidos en el codigo, luego, comparar con los nodos del grafo.
   * Si un metodo esta definido, pero no esta en el grafo ni en ningun arco como caller o calle se marca como inalcanzable.
   *
   * @param tree: AST generado por Scalameta
   * @param graphNodes: Conjunto de nodos del grafo.
   * @param methodImplementations: Mapeo que almacena las clases que implementan un método específico.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param unreachableNodes: Conjunto de nodos inalcanzables por el flujo del codigo fuente analizado.
   */
def addUnreachableNodes(
                         tree: Tree,
                         graphNodes: Set[String],
                         methodImplementations: Map[String, Set[String]],
                         graphEdges: Set[(String, String)],
                         unreachableNodes: scala.collection.mutable.Set[String]
                       ): Unit = {
  // Normalizar los nombres de los nodos alcanzables
  val normalizedGraphNodes = graphNodes.map(node => normalizeNodeName(node))

  // Recorrer el AST para encontrar todos los métodos definidos
  tree.collect {
    case defn: Defn.Def =>
      val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
      val paramTypes = defn.paramss.flatten.map(_.decltpe.map(_.syntax).getOrElse("_")).mkString(", ")
      val methodNode = buildNodeName(enclosingClassOrTrait, defn.name.value, paramTypes)

      // Normalizar el nombre del nodo
      val normalizedMethodNode = normalizeNodeName(methodNode)
      println(s"Nodo normalizado : $normalizedMethodNode")

      // Si el método no está en los nodos del grafo, agregarlo como nodo inalcanzable
      if (!normalizedGraphNodes.contains(normalizedMethodNode)) {
        unreachableNodes += methodNode
        println(s"Agregando nodo inalcanzable: $methodNode")
      }
  }

  // Eliminar nodos inalcanzables si existe una versión equivalente en graphNodes
  unreachableNodes.retain { unreachable =>
    val normalizedUnreachable = normalizeNodeName(unreachable)
    // Ignorar métodos como MainApp.main(Array[String])
    val isMainMethod = unreachable.contains("main") &&
      (unreachable.contains("Array[String]") || unreachable.endsWith("()"))

    if (isMainMethod) {
      println(s"Excluyendo método de entrada principal: $unreachable")
      false // Eliminar del conjunto de nodos inalcanzables
    } else {
      !graphNodes.exists { reachable =>
        val normalizedReachable = normalizeNodeName(reachable)
        normalizedReachable == normalizedUnreachable
      }
    }
  }
  println(s"Nodos inalcanzables finales: $unreachableNodes")
}

  /**
   * Dado un conjunto de nodos inalcanzables, se analizan los cuerpos de estos para encontrar llamados de metodos dentro de estos.
   * Luego, si un nodo inalcanzable llama a otro metodo, se agrega un arco entre estos.
   *
   * @param unreachableNodes: Conjunto de nodos inalcanzables por el flujo del codigo fuente analizado.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param tree: AST generado por Scalameta
   */

def processUnreachableNodes(
                             unreachableNodes: Set[String],
                             graphEdges: scala.collection.mutable.Set[(String, String)],
                             tree: Tree
                           ): Unit = {
  unreachableNodes.foreach { unreachableNode =>
    tree.collect {
      case defn: Defn.Def =>
        val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
        val paramTypes = defn.paramss.flatten.map(_.decltpe.map(_.syntax).getOrElse("_")).mkString(", ")
        val expectedNode = buildNodeName(enclosingClassOrTrait, defn.name.value, paramTypes)

        // Comparamos el nodo actual con el nodo inalcanzable
        if (normalizeNodeName(unreachableNode) == normalizeNodeName(expectedNode)) {
          println(s"Procesando nodo inalcanzable: $unreachableNode para nuevos métodos.")

          // Analizar el cuerpo del método inalcanzable
          defn.body.collect {
            case Term.Apply(Term.Name(calleeName), args) =>
              val argumentTypes = args.map(_.syntax).mkString(", ")
              val calleeNode = buildNodeName(enclosingClassOrTrait, calleeName, argumentTypes)

              if (!graphEdges.contains((unreachableNode, calleeNode))) {
                graphEdges += (unreachableNode -> calleeNode)
                println(s"Agregando arco desde inalcanzable: $unreachableNode -> $calleeNode")
              }

            case Term.Apply(Term.Select(Term.Name(objName), Term.Name(calleeName)), args) =>
              val dynamicType = objName // Puedes ajustar esto según la lógica de tu proyecto
              val argumentTypes = args.map(_.syntax).mkString(", ")
              val calleeNode = buildNodeName(dynamicType, calleeName, argumentTypes)

              if (!graphEdges.contains((unreachableNode, calleeNode))) {
                graphEdges += (unreachableNode -> calleeNode)
                println(s"Agregando arco desde inalcanzable: $unreachableNode -> $calleeNode")
              }
          }
        }
    }
  }
}


  /**
   * Dado un conjunto de arcos y un conjunto de nodos inalcanzables propaga la propiedad de estos nodos tal que se encuentre el flujo completo de
   * estos.
   *
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param unreachableNodes: Conjunto de nodos inalcanzables por el flujo del codigo fuente analizado.
   */
  def propagateUnreachability(
                               graphEdges: Set[(String, String)],
                               unreachableNodes: Set[String]
                             ): Unit = {
    var updated = true
    while (updated) {
      updated = false
      /**
       * Se recorre el conjunto de arcos y se filtran los arcos tal que:
       * El caller es un nodo inalcanzable Y el callee es un nodo que NO esta marcado como inalcanzable.
       * El resultado es un conjunto de nodos que deberan ser marcados como inalcanzables. Ejemplo:
       *      Unreachable.MethodTest1() -> Unreachable.MethodTest2()
       *      Unreachable.MethodTest2() -> Unreachable.println("Testing")
       * Si Unreachable.MethodTest1() es inalcanzable => Unreachable.MethodTest2() deberia ser marcado como inalcanzable.
       * Luego, por transitividad: Unreachable.println("Testing") tambien debe ser marcado como inalcanzable.
       */
      val newlyUnreachable = graphEdges.collect {
        case (caller, callee) if unreachableNodes.contains(caller) && !unreachableNodes.contains(callee) =>
          callee
      }
      // SI se identificaron nuevos nodos, entonces, marcarlos como inalcanzables.
      if (newlyUnreachable.nonEmpty) {
        newlyUnreachable.foreach { node =>
          unreachableNodes += node
          println(s"Marcando nodo como inalcanzable: $node")
        }
        updated = true
      }
    }
  }


  /**
   * Asegura que todos los nodos en el grafo (nodos finales, inalcanzables y arcos) tengan un formato consistente.
   *
   * @param graphNodes: Conjunto de nodos del grafo.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param unreachableNodes: Conjunto de nodos inalcanzables por el flujo del codigo fuente analizado.
   */
  def normalizeNodes(
                      graphNodes: Set[String],
                      graphEdges: Set[(String, String)],
                      unreachableNodes: Set[String]
                    ): Unit = {
    def normalize(node: String): String = {
      // Si el nodo ya tiene paréntesis o es una llamada con argumentos, no modificar
      if (node.endsWith("()") || node.contains("(")) node
      // Si el nodo parece ser un método o constructor, agregar paréntesis. Ej: Dog.makeSound -> Dog.makeSound()
      else if (node.contains(".")) s"$node()"
      else node
    }
    // Normalizar los nodos
    val normalizedNodes = graphNodes.map(normalize)
    val normalizedUnreachableNodes = unreachableNodes.map(normalize)
    val normalizedEdges = graphEdges.map { case (caller, callee) => (normalize(caller), normalize(callee)) }

    // Actualizar los conjuntos originales
    graphNodes.clear()
    graphNodes ++= normalizedNodes

    unreachableNodes.clear()
    unreachableNodes ++= normalizedUnreachableNodes

    graphEdges.clear()
    graphEdges ++= normalizedEdges
  }

  /**
   * Exporta el grafo a un archivo .dot para visualizar con Graphviz.
   */
  def exportGraphToDot(
                        graphNodes: Set[String],
                        graphEdges: Set[(String, String)],
                        unreachableNodes: Set[String],
                        fileName: String
                      ): Unit = {
    def sanitizeLabel(label: String): String = {
      // Escapar comillas dobles y caracteres problemáticos
      label.replace("\"", "\\\"")
    }

    // Asegurar que todos los nodos de unreachableNodes también estén en graphNodes
    val allNodes = graphNodes ++ unreachableNodes

    val writer = new PrintWriter(fileName)
    writer.println("digraph CallGraph {")
    writer.println("  node [shape=box];")

    // Agregar nodos con colores
    allNodes.foreach { node =>
      val color = if (unreachableNodes.contains(node)) "red" else "white"
      writer.println(s"""  "${sanitizeLabel(node)}" [style=filled, fillcolor=$color];""")
    }

    // Agregar arcos
    graphEdges.foreach { case (caller, callee) =>
      writer.println(s"""  "${sanitizeLabel(caller)}" -> "${sanitizeLabel(callee)}";""")
    }

    writer.println("}")
    writer.close()
  }

  /**
   *
   * @param directory
   * @return
   */
  def getScalaFiles(dir: String): List[File] = {
    val root = new File(dir)
    if (root.exists && root.isDirectory) {
      root.listFiles().flatMap { file =>
        if (file.isDirectory) {
          getScalaFiles(file.getAbsolutePath) // Llamada recursiva para carpetas
        } else if (file.getName.endsWith(".scala")) {
          List(file) // Archivo Scala encontrado
        } else {
          Nil
        }
      }.toList
    } else {
      Nil
    }
  }
}