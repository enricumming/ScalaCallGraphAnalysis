class AdvancedTest {
  def runTest(testing: Animal): Unit = {
    testing.makeSound()
  }

  def partialUsage(): Unit = {
    partiallyUsedMethod()
  }

  private def partiallyUsedMethod(): Unit = {
    println("This method is partially used.")
  }

  private def unusedMethod(): Unit = {
    println("This method is never called.")
  }
}
