object MainApp {
  def main(args: Array[String]): Unit = {
    val dog: Animal = new Dog("Buddy")
    dog.makeSound()

    // Uso de métodos de Utils
    Utils.printMessage("Welcome to the advanced test!")

   // Llamada a método polimórfico
    val advancedTest = new AdvancedTest
    advancedTest.runTest(dog)

    // Uso de una función lambda
    val logger = Utils.createLogger("AdvancedLogger")
    logger("This is a lambda-based log message.")

    // Métodos sobrecargados
    Utils.logSound()
    Utils.logSound("Custom sound")

    // Método parcialmente utilizado
    advancedTest.partialUsage()
  }
}
