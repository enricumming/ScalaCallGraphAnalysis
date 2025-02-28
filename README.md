# ScalaCallGraphAnalysis

### Descripción
Este proyecto tiene como objetivo optimizar los tiempos de compilación de un software escrito en Scala mediante la construcción de Call Graphs y la detección de código muerto. Se busca aplicar diferentes algoritmos de análisis de llamadas para identificar métodos y clases que no contribuyen al flujo de ejecución del programa y que pueden ser eliminados o refactorizados.

Para la implementación, se ha utilizado Scalameta para analizar el código fuente, Graphviz para visualizar los grafos generados y sbt como herramienta de compilación. 

Las versiones utilizadas fueron:
* Scala 2.12.10
* Java 8
* Sbt 1.3.3 

### Algoritmos Implementados

#### Name-Based Resolution (RA)

* Construye el Call Graph basándose únicamente en los nombres de los métodos.
* Considera que cualquier método con el mismo nombre podría ser alcanzado, sin verificar tipos.
* Es el algoritmo más rápido, pero también el menos preciso.

####  Class Hierarchy Analysis (CHA)

* Usa la jerarquía de clases para restringir los posibles métodos alcanzables.
*  Si un objeto llama a un método m, se consideran solo los métodos m definidos en la superclase o en sus subclases.
*  Más preciso que RA, pero puede incluir métodos que nunca se ejecutan en tiempo de ejecución.

#### Rapid Type Analysis (RTA)

* Refinamiento de CHA que solo considera clases que han sido instanciadas en el código.
*  Reduce falsos positivos de CHA.
* Es más preciso, pero depende de que el análisis encuentre todas las instancias posibles.

####  TCA Names

* Variante de RA que considera solo tipos instanciados en métodos alcanzables.
* Mejora la precisión sin requerir una jerarquía de clases.

####  TCA Bounds

* Extiende RTA para manejar tipos abstractos con cotas superiores.
* Si un tipo abstracto T tiene una cota superior B, se consideran llamadas a métodos de B y sus subclases.
* Reduce falsos positivos al considerar restricciones de tipo.

####  TCA Expand

Por implementar.

#### TCA Expand-This

Por implementar.

### Uso:

Para ejecutar el análisis en proyecto Scala:

  1. Clonar repositorio:

    git clone https://github.com/tu_usuario/tu_proyecto.git
    cd tu_proyecto

2. Instalar dependencias necesarias:
  
       sbt update
   
4. Chequear que sea versión de Java: java version "1.8.0_421"

       java -version

5. Inicializar build del proyecto con sbt:
   
       sbt -mem 3000

6. Ejecutar análisis ejemplo:

       runMain CHA <directorio_proyecto> <archivo_output.dot>

7. Visualiza el Call Graph generado usando Graphviz:

        dot -Tpng archivo_output.dot -o callgraph.png
