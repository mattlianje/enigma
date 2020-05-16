package enigma
import scala.util.control.Breaks._
import Rotors._
import Reflectors._
import Plugboards._
import Rotor._

object Main extends App {

  val test_machine = EngimaMachine(rotor_III('A'),
                                   rotor_II('A'),
                                   rotor_I('B'),
                                   UKW_B,
                                   test_plugboard)


  println("\nEnigma M3 as used by the Germans during WWII ...")
  println("\n(The M3 had three substitution rotors unlike the later four rotor M4 Kriegsmarine U-Boat Enigma)")
  test_machine.printMachineSpecs
  println("\n\n Input message you would like to encipher ... ")

  val user_input = scala.io.StdIn.readLine()
  val init = 0
  val res = ""

  runner(user_input, test_machine, init, res)

  def runner(str:String, machine:EngimaMachine, counter:Int, res:String): Unit = {
    // Retrieve machine with right state
    if (res.length == str.length) {
      println("\n Cipher text: " + res)
    }
    else {
      val next_machine = machine.getNextMachine
      val next_res = res + next_machine.encode(str.charAt(counter)).toString
      val next_counter = counter + 1
      runner(str, next_machine, next_counter, next_res)
    }
  }
}
