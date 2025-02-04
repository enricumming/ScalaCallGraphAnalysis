
object MainApp2 {
  def main(args: Array[String]): Unit = {
    val dog: Animal = new Dog("Buddy")
    dog.makeSound() // Direct call to a polymorphic method

    Utils.printMessage("Welcome to the testing environment!")

    // Example of a lambda
    val logger = Utils.createLogger("MainLogger")
    logger("This is a log message.")

   // Example of reflection
    val dynamicClass = "animals.Bird"
    advanced.DynamicCalls.invokeMakeSound(dynamicClass)
  }
}
