object TestRA {
  def main(args: Array[String]): Unit = {
    val dog: Animal = new Dog()
    val cat: Animal = new Cat()
    val robot = new Robot()

    dog.makeSound()
    cat.makeSound()
    robot.makeSound()
  }

  trait Animal {
    def makeSound(): Unit
  }

  class Dog extends Animal {
    override def makeSound(): Unit = println("Woof!")
  }

  class Cat extends Animal {
    override def makeSound(): Unit = println("Meow!")

  }

  class Robot {
    def makeSound(): Unit = println("Beep!")
  }

}
