class Dog(name: String) extends Animal(name) {
  def makeSound(): Unit = {
    println(s"$name says: Woof!")
    Utils.logSound("Woof!")
  }

  // Método sobrecargado
  def makeSound(volume: Int): Unit = {
    println(s"$name says: Woof! with volume $volume")
  }
}
