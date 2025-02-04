import scala.meta._
import scala.collection.mutable.{Set, Map}
import scala.io.{Source => IOSource}
import java.io.PrintWriter
import java.io.File

object CHA2 {
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
    val sourceTrees = scalaFiles.map { file =>
      val code = IOSource.fromFile(file).mkString
      code.parse[Source].get
    }

    //    println("Estructura del AST del código:")
    //    println(source.structure)

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
    val graphNodes = Set[String]()
    val graphEdges = Set[(String, String)]()
    val unreachableNodes = Set[String]()

    // Analizar cada archivo
    sourceTrees.foreach { source =>
      buildClassHierarchy(source, classHierarchy, methodImplementations)
      generateGraphFromAST(source, graphNodes, graphEdges, classHierarchy, variableTypes)
    }

    // Aplicar CHA para agregar nodos y arcos adicionales
    applyCHA(graphNodes, graphEdges, classHierarchy, methodImplementations, sourceTrees, variableTypes)

    println(s"El conjunto de NODOS ACTUAL ES : $graphNodes")

    // Buscar nuevos métodos llamados desde los nodos finales del grafo en todos los archivos
    sourceTrees.foreach { source =>
      findCalleesFromGraphNodes(graphNodes, graphEdges, source, variableTypes)
    }

    // Marcar nodos inalcanzables
    sourceTrees.foreach { source =>
      addUnreachableNodes(source, graphNodes, methodImplementations, graphEdges, unreachableNodes)
      processUnreachableNodes(unreachableNodes, graphEdges, source)
      propagateUnreachability(graphEdges, unreachableNodes)
    }

    //  Normalizar nodos y arcos
    normalizeNodes(graphNodes, graphEdges, unreachableNodes)

    // Exportar a archivo .dot
    exportGraphToDot(graphNodes, graphEdges, unreachableNodes, outputFilePath)

    println(s"\nGrafo exportado a '$outputFilePath'")

    println(s"Los nodos encontrados son : $graphNodes")

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
        //        println(s"La clase es: $className, y esta extiende a: ${defn.templ.inits}")
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
                            graphNodes: Set[String],
                            graphEdges: Set[(String, String)],
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

        //Detecta llamadas de metodos de un objeto. Ej: dog.makeSound()
        // Agrega las llamadas encontradas como un nodo junto al objeto que hace el llamado. Luego, agrega un arco desde el nodo
        // caller al nodo encontrado.
        case Term.Apply(Term.Select(Term.Name(valueName), Term.Name(methodName)), args) =>
          //Obtiene el contexto u objeto que hace la llamada.
          val contextType = variableTypes.getOrElse(valueName, valueName)

          //          val methodWithType = s"$contextType.$methodName"
          val methodWithType = if (args.nonEmpty) {
            s"$contextType.$methodName(${args.map(_.syntax).mkString(", ")})"
          } else {
            s"$contextType.$methodName()"
          }

          println(s"METODO ENCONTRADO CON CONTEXTO: $methodWithType")
          graphNodes += methodWithType
          addEdge(caller, methodWithType)

          //Recorre los argumentos de las llamadas a metodos y se procesan recursivamente. Permite recorrer el flujo del codigo fuente.
          // Permite encontrar llamadas a metodos anidadas, flujos indirectos, etc.
          args.foreach(arg => processNode(arg, methodWithType))

        // Detecta llamadas de metodos directas junto a su argumento. Ej: println("...")
        // Se agrega al Set de nodos, y luego se agrega un arco correspondiente.
        case Term.Apply(Term.Name(methodName), args) =>
          val detailedMethodCall = s"$methodName(${args.map(_.syntax).mkString(", ")})"
          println(s"METODO ENCONTRADO TEST: $detailedMethodCall")
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

        //Detecta una declaracion de valor. Ej: val dog: Animal = new Dog().
        //varName: dog, typeName: Animal, init: new Dog()
        case Defn.Val(_, List(Pat.Var(Term.Name(varName))), Some(Type.Name(typeName)), init) =>
          variableTypes(varName) = typeName
          processNode(init, caller)

        //Si el currentNode no coincide con ningun patron anterior, recorre los nodos hijos del nodo actual y los procesa.
        case _ =>
          currentNode.children.foreach(child => processNode(child, caller))
      }
    }
    // Cuando encuentra el metodo main en el AST lo procesa como un nodo inicial.
    tree.collect {
      case defn: Defn.Def if defn.name.value == "main" => processNode(defn, "main")
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
                graphNodes: Set[String],
                graphEdges: Set[(String, String)],
                classHierarchy: Map[String, Set[String]],
                methodImplementations: Map[String, Set[String]],
                sourceTrees: List[Tree],
                variableTypes: Map[String, String]
              ): Unit = {
    val newEdges = Set[(String, String)]()

    graphEdges.foreach { case (caller, callee) =>
      callee.split("\\.") match {
        case Array(contextType, methodName) =>
          findAllSubtypes(contextType, classHierarchy).foreach { subtype =>
            if (methodImplementations.getOrElse(methodName, Set()).contains(subtype)) {
              val subtypeMethod = s"$subtype.$methodName"
              if (!graphNodes.contains(subtypeMethod)) {
                graphNodes += subtypeMethod
                newEdges += (caller -> subtypeMethod)
                println(s"Agregando nodo para subtipo transitivo: $subtypeMethod")

                //  Revisar el cuerpo del método recién agregado
                sourceTrees.foreach { tree =>
                  findCalleesFromGraphNodes(Set(subtypeMethod), newEdges, tree, variableTypes)
                }
              }
            }
          }
        case _ =>
          println(s"Advertencia: formato inesperado en callee '$callee'")
      }
    }
    // Se actualiza el conjunto de arcos.
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
   * Recorre el AST en busca de llamados de metodos  desde los nodos finales del grafo.
   *
   * @param graphNodes: Conjunto de nodos del grafo.
   * @param graphEdges: Conjunto de arcos del grafo (caller -> callee).
   * @param tree: AST generado por Scalameta
   * @param variableTypes: Mapeo que permite conocer el tipo de las variables inicializadas en el codigo fuente
   */
    def findCalleesFromGraphNodes(
                                   graphNodes: Set[String],
                                   graphEdges: Set[(String, String)],
                                   tree: Tree,
                                   variableTypes: Map[String, String]
                                 ): Unit = {
      // Se recorre cada nodo del conjunto graphNodes.
      graphNodes.foreach { node =>
        tree.collect {
          //Se buscan definiciones de metodos que coincidan con el nombre del nodo. Ej: dog.makeSound(), se busca en el arbol una definicion de metodo makeSound()
          case defn: Defn.Def if node.endsWith(defn.name.value) =>
            // Identifica la clase, objeto o Trait que implementa el metodo, luego se crea un nodo con esto.
            val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
            val expectedNode = s"$enclosingClassOrTrait.${defn.name.value}"

            if (node == expectedNode) {
              println(s"Revisando cuerpo de $node para nuevos métodos.")
              // Se recorre el cuerpo del metodo para buscar otras llamadas a metodos.
              defn.body.collect {
                    //Capturar llamadas directas
                case Term.Apply(Term.Name(methodName), args) =>
                  if (args.size == defn.paramss.flatten.size ) {
                    val argumentContent = args.map(_.syntax).mkString(", ")
                    val calleeContext = findEnclosingClassOrTrait(defn, tree)
                    val newCallee = s"$calleeContext.$methodName($argumentContent)"
                    println(s"Se encontro el metodo directo: $newCallee")

                    //Se agrega el arco al grafo si es que no existia previamente
                    if (!graphEdges.contains((node, newCallee))) {
                      graphEdges += (node -> newCallee)
                      println(s"Agregando arco: $node -> $newCallee")
                    }

                    // Agregar el nuevo nodo al conjunto de nodos
                    if (!graphNodes.contains(newCallee)) {
                      graphNodes += newCallee
                      println(s"Agregando nodo: $newCallee")
                    }
                  }

                // Caso de llamada con prefijo de objeto (Utils.logSound)
                case Term.Apply(Term.Select(Term.Name(objectName), Term.Name(methodName)), args) =>
                  val dynamicType = variableTypes.getOrElse(objectName, objectName)
                  val newCallee = s"$dynamicType.$methodName"
                  println(s"Se encontro el metodo con prefijo: $newCallee")
                  if (!graphEdges.contains((node, newCallee))) {
                    graphEdges += (node -> newCallee)
                    println(s"Agregando arco: $node -> $newCallee")
                  }

                  // Agregar el nuevo nodo al conjunto de nodos
                  if (!graphNodes.contains(newCallee)) {
                    graphNodes += newCallee
                    println(s"Agregando nodo: $newCallee")
                  }
              }
            }
        }
      }
    }
//  def findCalleesFromGraphNodes(
//                                 graphNodes: Set[String],
//                                 graphEdges: Set[(String, String)],
//                                 tree: Tree,
//                                 variableTypes: Map[String, String]
//                               ): Unit = {
//    val processedNodes = scala.collection.mutable.Set[String]() // Evitar duplicados
//
//    graphNodes.foreach { node =>
//      if (!processedNodes.contains(node)) {  // Procesar solo si no ha sido procesado
//        tree.collect {
//          // Buscar definiciones de métodos que coincidan con el nombre del nodo actual
//          case defn: Defn.Def if node.startsWith(findEnclosingClassOrTrait(defn, tree)) =>
//            val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
//            val expectedNode = s"$enclosingClassOrTrait.${defn.name.value}"
//
//            if (node.startsWith(expectedNode)) {
//              println(s"Revisando cuerpo de $node para nuevos métodos.")
//              processedNodes += node // Marcar como procesado
//
//              defn.body.collect {
//                // Capturar llamadas directas
//                case Term.Apply(Term.Name(methodName), args) =>
//                  // Construir el nodo del método callee con sus argumentos
//                  val argumentContent = args.map(_.syntax).mkString(", ")
//                  val calleeNode = s"$enclosingClassOrTrait.$methodName($argumentContent)"
//                  println(s"Se encontró método directo: $calleeNode")
//                  addEdgeAndNode(node, calleeNode, graphNodes, graphEdges)
//
//                // Capturar llamadas con prefijo de objeto
//                case Term.Apply(Term.Select(Term.Name(objectName), Term.Name(methodName)), args) =>
//                  val dynamicType = variableTypes.getOrElse(objectName, objectName)
//                  val argumentContent = args.map(_.syntax).mkString(", ")
//                  val calleeNode = s"$dynamicType.$methodName($argumentContent)"
//                  println(s"Se encontró método con prefijo: $calleeNode")
//                  addEdgeAndNode(node, calleeNode, graphNodes, graphEdges)
//              }
//            }
//        }
//      }
//    }
//  }

  /**
   * Funcion aux
   *
   * @param caller
   * @param callee
   * @param graphNodes
   * @param graphEdges
   */
  def addEdgeAndNode(
                      caller: String,
                      callee: String,
                      graphNodes: Set[String],
                      graphEdges: Set[(String, String)]
                    ): Unit = {
    if (!graphEdges.contains((caller, callee))) {
      graphEdges += (caller -> callee)
      println(s"Agregando arco: $caller -> $callee")
    }
    if (!graphNodes.contains(callee)) {
      graphNodes += callee
      println(s"Agregando nodo: $callee")
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
    findEnclosing(tree).getOrElse("Unknown")
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
                           unreachableNodes: Set[String]
                         ): Unit = {
    // Recorrer el AST para encontrar todos los métodos definidos
    tree.collect {
      case defn: Defn.Def =>
        val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
        val methodNode = s"$enclosingClassOrTrait.${defn.name.value}"

        // Si el método no está en los nodos del grafo, agregarlo como nodo inalcanzable
        if (!graphNodes.contains(methodNode) ) {
          unreachableNodes += methodNode
          println(s"Agregando nodo inalcanzable: $methodNode")
        }
    }
    // Eliminar nodos que están presentes en los arcos como caller o callee
    //Se recorre el conjunto de arcos para identificar todos los nodos que aparecen como caller o callee. Los nodos que aparecen se eliminan del conjunto
    // unreachableNodes.
    //    val reachableNodes = graphEdges.flatMap { case (caller, callee) => Set(caller, callee) }
    //    unreachableNodes --= reachableNodes
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
                               graphEdges: Set[(String, String)],
                               tree: Tree
                             ): Unit = {
    unreachableNodes.foreach { node =>
      tree.collect {
        case defn: Defn.Def if node.endsWith(defn.name.value) =>
          val enclosingClassOrTrait = findEnclosingClassOrTrait(defn, tree)
          val expectedNode = s"$enclosingClassOrTrait.${defn.name.value}"

          if (node == expectedNode) {
            println(s"Procesando nodo inalcanzable: $node para nuevos métodos.")
            defn.body.collect {
              case Term.Apply(Term.Name(methodName), args) =>
                val argumentContent = args.map(_.syntax).mkString(", ")
                val calleeContext = findEnclosingClassOrTrait(defn, tree)
                val newCallee = s"$calleeContext.$methodName($argumentContent)"

                if (!graphEdges.contains((node, newCallee))) {
                  graphEdges += (node -> newCallee)
                  println(s"Agregando arco desde inalcanzable: $node -> $newCallee")
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