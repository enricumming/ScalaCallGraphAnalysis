package example

object Example3 {
  trait A {
    type T <: A  // Tipo abstracto restringido a ser subtipo de A
    def foo: String
  }

  class B extends A {
    type T = B  // Se concreta el tipo abstracto T como B
    override def foo: String = "B.foo"
  }

  class C extends A {
    type T = C  // Se concreta T como C
    override def foo: String = "C.foo"
  }

  class D {
    def foo: String = "D.foo"
  }

  class CallSiteClass[T](val receiver: T) {
    def callsite: String = {
      receiver.foo
    }
  }

  def main(args: Array[String]): Unit = {
    new A  // Instancia anónima de A para ver cómo la trata el análisis
    val receiver = new B
    new C
    new D  // Agregado pero no debería ser considerado en el call graph por TCA Bounds

    val callSiteClass = new CallSiteClass[B](receiver)
    callSiteClass.callsite()
  }
}

