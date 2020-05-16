package enigma

case class EngimaMachine(
                        rightRotor: Rotor,
                        middleRotor: Rotor,
                        leftRotor: Rotor,
                        reflector: Reflector,
                        plugboard: Option[String] = None
                        ) {

  def parsePlugboard(connections: String) = {
    // TODO
  }

  def encode(c: Char) = {
    val right_rotor_encryption = rightRotor.encrypt(c)
    val middle_rotor_encryption = middleRotor.encrypt(right_rotor_encryption)
    val left_rotor_encryption = leftRotor.encrypt(middle_rotor_encryption)
    val reflector_encryption = reflector.encrypt(left_rotor_encryption)
    val left_rotor_reflected = leftRotor.reverseEncrypt(reflector_encryption)
    val middle_rotor_reflected = middleRotor.reverseEncrypt(left_rotor_reflected)
    val right_rotor_reflected = rightRotor.reverseEncrypt(middle_rotor_reflected)
    println(left_rotor_encryption + " <- " + middle_rotor_encryption + " <- " + right_rotor_encryption + " <- " + "input: " + c + "\n" +
      reflector_encryption + " | " + "\n" +
      left_rotor_reflected + " -> " + middle_rotor_reflected + " -> " + right_rotor_reflected + " -> " + "output: " + right_rotor_reflected)
    right_rotor_reflected
  }
}
