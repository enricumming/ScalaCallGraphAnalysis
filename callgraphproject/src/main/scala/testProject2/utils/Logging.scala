package utils

object Logging {
  def logInfo(message: String): Unit = {
    println(s"[INFO] $message")
  }

  def logError(error: String): Unit = {
    println(s"[ERROR] $error")
  }

  private def logDebug(debug: String): Unit = {
    println(s"[DEBUG] $debug")
  }
}
