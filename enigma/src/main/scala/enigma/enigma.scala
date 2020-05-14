package enigma
import scala.util.control.Breaks._
import Rotors._
import Rotor._

object Main extends App {
    val test_rotor = rotor_I('A')
    val new_rotor = test_rotor.turnRotor
    println(new_rotor.position, new_rotor.getOffset)
}

object Helpers {
  val alphabet: Seq[Char] = ('A' to 'Z').toList
  println(alphabet)
}