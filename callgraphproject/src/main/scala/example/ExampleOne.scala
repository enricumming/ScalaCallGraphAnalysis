package example

object ExampleOne {
  def main(args: Array[String]): Unit = {
    functionA()
    functionB()
  }

  def functionA(): Unit = {
    println("Function A")
    functionC()
  }

  def functionB(): Unit = {
    println("Function B")
    functionC()
    functionD()
  }

  def functionC(): Unit = {
    println("Function C")
  }

  def functionD(): Unit = {
    println("Function D")
    functionE()
  }

  def functionE(): Unit = {
    println("Function E")
  }
}


//object Traits {
//  trait A {
//    def foo = println("A.foo")
//    def bar
//  }
//  trait B {
//    def foo
//    def bar = this.foo
//  }
//
//  def main(args: Array[String]) = {
//    (new A new B).bar
//  }

//object AbstractTypeMembers {
//  // Trait base, se define un metodo abstracto foo
//  // Notar que, si una clase extiende de este trait entonces DEBE SI O SI implementar foo
//  trait HasFoo {
//    def foo: Unit
//  }
//
//  // Trait X con una clase interna A que extiende el trait HasFoo
//  trait X {
//    class A extends HasFoo {
//      def foo = println("X.A.foo")
//    }
//  }
//
//  // Trait Y con clase interna A que extiende el trait HasFoo
//  // type B : crea un alias B que hace referencia a la clase A definida en Y
//  // QUE ES UN TYPE? ... Se usa para definir un alias de tipos o tipos abstractos, por ejemplo type B no crea una instancia de A
//  // solo proporciona otra forma de referirse a esta. Cualquier referencia a B == cualquier referencia a A
//  // val o : crea una instancia de la clase A
//  trait Y {
//    class A extends HasFoo {
//      def foo = println("Y.A.foo")
//    }
//
//    type B = A
//    val o = new A
//  }
//  // Trait Z, que es el simbolo <: y :
//  // Simbolo <:  -> Upper bound.
//  // Significa que el tipo a la izq del simbolo debe ser un subtipo del tipo de la derecha
//  // Para este caso, quiere decir que el tipo B (tipo abstracto) esta acotado sup por HasFoo. Lo que garantiza que
//  // B tendra acceso a los metodos y campos definidos por HasFoo
//
//  // Simbolo : -> Type Ascription
//  // Se usa para ESPECIFICAR el tipo de una variable
//
//  trait Z {
//    type B <: HasFoo
//    val o: B
//
//    def bar = o.foo
//  }
//
//  // Se combina Y con Z
//  // Z.bar -> o.foo -> B.foo, luego como type B <: HasFoo, entonces B debe tener implementado el metodo foo.
//  // En el trait Z no esta definido foo, luego, se busca en el trait Y ya que estos estas combinados
//  // En el trait Y, la clase A implementa el Foo.
//  // Luego el resultado es : "Y.A.foo"
//  def main(args: Array[String]) = {
//    (new Y with Z {}).bar
//  }
//
//  /////////////////////////////////////////
//  /////////////////////////////////////////
//
//  object Closures {
//    def bar1(y: () => A) = {
//      y()
//    }
//
//    def bar2(z: () => B) = {
//      z()
//    }
//  }
//
//  class A
//  class B
//
//  def main(args: Array[String]) = {
//    val foo1 = () => {
//      new A
//    }
//    val foo2 = () => {
//      new B
//    }
//    this.bar1(foo1)
//    this.bar2(foo2)
//  }

//}