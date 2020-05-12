package enigma

object Hello extends Greeting with App {
  println(greeting)
  val test = List(1, 2, 3, 4)

  test.foreach {
    println
  }
}

trait Greeting {
  lazy val greeting: String = "hello"
}


object Helpers {
  val alphabet: Seq[Char] = ('A' to 'Z').toList
  println(alphabet)
}