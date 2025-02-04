package advanced

object DynamicCalls {
  def invokeMakeSound(className: String): Unit = {
    try {
      val clazz = Class.forName(className)
      val constructor = clazz.getConstructor(classOf[String])
      val instance = constructor.newInstance("DynamicBird").asInstanceOf[animals.Animal]
      instance.makeSound()
    } catch {
      case e: Exception => println(s"Error during reflection: ${e.getMessage}")
    }
  }
}
