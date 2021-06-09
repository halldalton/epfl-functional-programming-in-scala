package week1

class HelloThread extends Thread {

  override def run(): Unit = {
    println("Hello ")
    println("world!")
  }
  
}
