package deprecated

@deprecated("This class is no longer used.", "1.0")
class DeprecatedClass {
  @deprecated("This method is no longer called.", "1.0")
  def deprecatedMethod(): Unit = {
    println("This is a deprecated method.")
  }
}
