package animals

class Dog(name: String) extends Animal(name) {
  def makeSound(): Unit = {
    println(s"$name says: Woof!")
    utils.Logging.logInfo(s"$name barked.")
  }
}
