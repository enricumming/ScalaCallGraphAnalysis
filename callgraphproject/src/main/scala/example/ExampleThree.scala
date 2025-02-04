object ExampleThree {

  trait Animal { def makeSound(): Unit }
//  trait NoAnimal {def makeSound(): Unit}
  class Dog extends Animal { override def makeSound(): Unit = println("Woof!") }
  class Cat extends Animal { override def makeSound(): Unit = println("Meow!") }
  class Akita extends Dog { override def makeSound(): Unit = println("Hachi!")}
//  class Robot extends NoAnimal { override def makeSound(): Unit = println("Beep!") }
  class Robot {def makeSound(): Unit = println("Beep!")}
//  class Blitzcrank extends Robot {override def makeSound(): Unit = println("Testing")}

  class Unreachable {
    def MethodTest1(): Unit = {
      MethodTest2()
    }
    def MethodTest2(): Unit = {
      MethodTest3()
    }
    def MethodTest3(): Unit = {
      println("Testing")
    }
  }

  def main(args: Array[String]): Unit = {
    val dog: Animal = new Dog()
    dog.makeSound()

//    val robot: NoAnimal = new Robot()
//    robot.makeSound()
  }
// Lo que quiero obtener:
  /**
   * main -> Dog.<init>
   * main -> Animal.makeSound()
   * main -> Dog.makeSound()
   * main -> Cat.makeSound()
   * Dog.makeSound() -> println("Woof!")
   * Cat.makeSound() -> println("Meow!")
   */

  /**
   * Si es que agrego Trait NoAnimal y clase Robot, Blitz. El grafo que deberia obtener es:
   * main -> Dog.<init> OK
   * main -> Animal.makeSound() OK
   * main -> NoAnimal.makeSound() OK
   * main -> Dog.makeSound() OK
   * main -> Cat.makeSound() OK
   * Dog.makeSound() -> println("Woof!") OK
   * Cat.makeSound() -> println("Meow!") OK
   * SE agregan estos nodos y arcos:
   * main -> Robot.<init> OK
   * main -> Blitzcrank.makeSound() OK
   * main -> Robot.makeSound() OK
   * Robot.makeSound() -> println("Beep") OK
   * Blitzcrank.makeSound() -> println("Testing") OK
   */
}
