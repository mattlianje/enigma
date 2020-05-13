package enigma
import Rotors._

object Hello extends Greeting with App {
  println(greeting)
  val test = List(1, 2, 3, 4)
  val test_rotor = rotor_I('F')
  // println(test_rotor.get_offset_alphabet('E'))
  val new_rotor = test_rotor.change_pos_test
  println(new_rotor.position)
}

trait Greeting {
  lazy val greeting: String = "hello"
}


object Helpers {
  val alphabet: Seq[Char] = ('A' to 'Z').toList
  println(alphabet)
}