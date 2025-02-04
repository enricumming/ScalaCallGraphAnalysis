package animals

class Bird(name: String) extends Animal(name) {
  def makeSound(): Unit = {
    println(s"$name chirps melodiously.")
  }
}
