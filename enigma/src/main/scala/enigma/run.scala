package enigma
import scala.util.control.Breaks._
import Rotors._
import Reflectors._
import Rotor._

object Main extends App {

  val test_machine = EngimaMachine(rotor_I('D'), rotor_II('A'), rotor_III('A'), UKW_B)
  println(test_machine.encode('G').toString())

}

object Helpers {
  val alphabet: Seq[Char] = ('A' to 'Z').toList
  println(alphabet)
}