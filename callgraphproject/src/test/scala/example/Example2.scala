//package example
//
//object Example2 {
//
//  trait E {
//    def foo2: String
//  }
//
//  trait H {
//    def bar2: String
//  }
//
//  class F extends E {
//    override def foo2 = "F.foo"
//  }
//
//  class G extends E with H { //  `G` extiende E y H
//    override def foo2 = "G.foo"
//    override def bar2 = "G.bar"
//  }
//
//  class CallSiteClass2[T <: E with H](val receiver: T) {
//    def callsite2 = {
//      receiver2.foo2
//      receiver2.bar2
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    val receiver2 = new G()
//    val callSiteClass2 = new CallSiteClass2[G](receiver)
//    callSiteClass2.callsite2()
//  }
//}