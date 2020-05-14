package enigma

case class EngimaMachine(
                        rightRotor: Rotor,
                        middleRotor: Rotor,
                        leftRotor: Rotor,
                        reflector: String,
                        plugboard: String
                        ) {

  val reflector_UKW_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
  val reflector_UKW_C = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

}
