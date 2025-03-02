//package example
//
//object Example {
//
//  class A {
//    def foo = "A.foo"
//  }
//
//  class B extends A {
//    override def foo = "B.foo"
//  }
//
//  class C {
//    def foo = "C.foo"
//  }
//
//  class D {
//    def foo = "D.foo"
//  }
//
//  class CallSiteClass[T <: A](val receiver: T) {
//    def callsite = {
//      receiver.foo
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    new A
//    val receiver = new B
//    new C
//    val callSiteClass = new CallSiteClass[B](receiver);
//    callSiteClass.callsite()
//  }
//}


  /**
   * Se crean instancias de A, B y C, pero nunca de D.
   * TCA Bounds resuelve el llamado callsite a: A.foo y B.foo
   * C no es subtipo de A, por lo que no es llamado
   * D no es instanciado por lo que D.foo no es considerado
   * TCA Bounds entonces, hace un analisis estatico de tipos en el que:
   * Se aceptan subtipos del tipo estatico
   * Considera solo las clases instanciadas
   * Usa el upper bound para restringir tipos genericos!
   *
   * Reglas TCA Bounds:
   * Tipos abstractos no tienen subtipos por definicion en Scala, pero, cada clase abstracta tiene un upper bound por defecto
   * que si no se especifica es scala.Any
   */
