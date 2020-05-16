package enigma

case class EngimaMachine(
                        leftRotor: Rotor,
                        middleRotor: Rotor,
                        rightRotor: Rotor,
                        reflector: Reflector,
                        plugboard: Plugboard
                        ) {

  def parsePlugboard(connections: String) = {
    // TODO
  }

  def encode(c: Char) = {

    val input_through_plugboard = plugboard.passThruPlugboard(c)
    val right_rotor_encryption = rightRotor.encrypt(input_through_plugboard)
    val middle_rotor_encryption = middleRotor.encrypt(right_rotor_encryption)
    val left_rotor_encryption = leftRotor.encrypt(middle_rotor_encryption)
    val reflector_encryption = reflector.encrypt(left_rotor_encryption)
    val left_rotor_reflected = leftRotor.reverseEncrypt(reflector_encryption)
    val middle_rotor_reflected = middleRotor.reverseEncrypt(left_rotor_reflected)
    val right_rotor_reflected = rightRotor.reverseEncrypt(middle_rotor_reflected)
    val reflected_plugboard = plugboard.passThruPlugboard(right_rotor_reflected)

    println("\n" + " Rotor states (L,M,R): " + "   " + leftRotor.position + "   " + middleRotor.position + "   " + rightRotor.position)
    println("\n" + " Reflector  " + " Left " + " Middle " + " Right " + "   Plugboard" + "\n\n" +
      "               " + left_rotor_encryption + " <-- " + middle_rotor_encryption + " <-- " + right_rotor_encryption + " <--     " + input_through_plugboard + " <-- " + "input: " + c + "\n" +
      "      " + reflector_encryption + " | " + "\n" +
      "               " + left_rotor_reflected + " --> " + middle_rotor_reflected + " --> " + right_rotor_reflected + " -->     " +  reflected_plugboard + " --> " + "output: " + reflected_plugboard)
    reflected_plugboard
  }

  def getNextMachine: EngimaMachine = {
    if (middleRotor.position == middleRotor.notch && rightRotor.position == rightRotor.notch) {
      val res = EngimaMachine(leftRotor.turnRotor, middleRotor.turnRotor, rightRotor.turnRotor, reflector, plugboard)
      res
    }
    else if (rightRotor.position == rightRotor.notch) {
      val res = EngimaMachine(leftRotor, middleRotor.turnRotor, rightRotor.turnRotor, reflector, plugboard)
      res
    }
    else {
      val res = EngimaMachine(leftRotor, middleRotor, rightRotor.turnRotor, reflector, plugboard)
      res
    }
  }

  def printMachineSpecs = {
    println("\n Machine settings:\n")
    println("                         start_position")
    println("   left rotor: " + leftRotor.model + " .... " + leftRotor.position)
    println("   middle rotor: " + middleRotor.model + " .... " + middleRotor.position)
    println("   right rotor: " + rightRotor.model + " .... " + rightRotor.position)
    println("\n reflector:   " + reflector.model)
  }
}
