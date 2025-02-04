object Utils {
  def printMessage(message: String): Unit = {
    println(s"Message: $message")
  }

  def logSound(): Unit = {
    println("Logging default sound.")
  }

  def logSound(sound: String): Unit = {
    println(s"Logging sound: $sound")
  }

  // Función que retorna una función lambda
  def createLogger(prefix: String): String => Unit = {
    message => println(s"[$prefix] $message")
  }
}
