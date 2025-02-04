package utils

object Utils {
  def printMessage(message: String): Unit = {
    println(s"Message: $message")
  }

  // Returns a lambda function
  def createLogger(prefix: String): String => Unit = {
    message => println(s"[$prefix] $message")
  }
}
