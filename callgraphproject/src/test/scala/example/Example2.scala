package example

object Example2 {

  trait A {
    def foo: String
  }

  trait D {
    def bar: String
  }

  class B extends A {
    override def foo = "B.foo"
  }

  class C extends A with D { //  `C` extiende A y D
    override def foo = "C.foo"
    override def bar = "C.bar"
  }

  class CallSiteClass[T <: A with D](val receiver: T) {
    def callsite = {
      receiver.foo
      receiver.bar
    }
  }

  def main(args: Array[String]): Unit = {
    val receiver = new C()
    val callSiteClass = new CallSiteClass[C](receiver)
    callSiteClass.callsite()
  }
}