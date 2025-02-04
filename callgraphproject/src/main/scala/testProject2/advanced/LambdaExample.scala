package advanced

object LambdaExample {
  val unusedLambda: String => String = { input =>
    input.toUpperCase
  }

  def unusedMethod(): Unit = {
    println("This method is not called.")
  }
}
