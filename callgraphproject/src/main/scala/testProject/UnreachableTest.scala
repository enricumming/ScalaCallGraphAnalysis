object UnreachableTest {
  def unusedMethod(): Unit = {
    println("This method is never called.")
  }

  def anotherUnusedMethod(): Unit = {
    println("Another unused method.")
  }

  def partiallyUsedMethod(): Unit = {
    println("This method is partially used.")
  }

  def callPartialMethod(): Unit = {
    partiallyUsedMethod()
  }
}