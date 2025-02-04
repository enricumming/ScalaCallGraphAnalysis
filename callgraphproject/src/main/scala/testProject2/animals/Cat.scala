package animals

class Cat(name: String) extends Animal(name) {
  def makeSound(): Unit = {
    println(s"$name says: Meow!")
    utils.Logging.logInfo(s"$name meowed.")
  }
}
