package example

trait Animal {
  def makeSound(): Unit
}

class Dog extends Animal {
  override def makeSound(): Unit = println("Woof!")
}

class Cat extends Animal {
  override def makeSound(): Unit = println("Meow!")
}

object Test {
  def main(args: Array[String]): Unit = {
    val animals: List[Animal] = List(new Dog, new Cat)
    animals.foreach(_.makeSound())
  }
}

