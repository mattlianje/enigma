package enigma

case class Rotor(
                  // letter_map <- immutable sequence of chars that defines the rotor ...
                  letter_roll: String,
                  // position <- current char the rotor is at ...
                  position: Char,
                  // notch <- immutable attribute of the rotor type (i.e. for a II rotor, the notch is always the same) ...
                  notch: Char,
                  // ring <- wiring offset
                  ring: Char = 'A') {
  /*
   *  NOTCH:
   *
   *  https://en.wikipedia.org/wiki/Enigma_rotor_details
   *  Example:
   *    Rotor | Notch  | Effect
   *  _________________________________________________________________________
   *    II    | E      | If rotor steps from E to F, the next rotor is advanced
   *
   *  RING:
   *
   *  Rotor I starting with ring setting A-1:
   *  EKMFLGDQVZNTOWYHXUSPAIBRCJ
   *  abcdefghijklmnopqrstuvwxyz
   *
   *  Rotor I starting with ring setting B-2:
   *  EKMFLGDQVZNTOWYHXUSPAIBRCJ
   *  zabcdefghijklmnopqrstuvwxy
   *
   */

  val alphabet_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val ring_int: Int = getPositionOf(alphabet_str, ring)
  val position_int: Int = getPositionOf(alphabet_str, position)
  val offset: Int = position_int - ring_int
  val alphabet_size = 26

  /*
   * turnRotor inspired by: https://medium.com/zyseme-technology/functional-references-lens-and-other-optics-in-scala-e5f7e2fdafe
   */
  def turnRotor: Rotor = this.copy(
    position = nextLetter(position.toString)
  )

  def nextLetter(x:String) : Char = {
    if (x == "Z") {
      val end = 'A'
      end
    }
    else {
      (x(0) + 1).toChar
    }
  }

  def getOffset: Int = {
    offset
  }

  /*
   * get_position_of: Gets the position of an element in list ...
   * @input <- String, Char
   * @output -> Int
   */
  def getPositionOf(input_string: String, c: Char): Int = {
    val new_list = input_string.toList
    new_list.indexOf(c) + 1
  }
}

  /*
   * M3 Enigma rotors
   * https://www.cryptomuseum.com/crypto/enigma/m3/index.htm
   */
  object Rotors {
    def rotor_I(p: Char) = Rotor(
      letter_roll  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
      position = p,
      notch  = 'R',
      ring   = 'A'
    )
    def rotor_II(p: Char) = Rotor(
      letter_roll  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
      position = p,
      notch  = 'F',
      ring   = 'A'
    )
    def rotor_III(p: Char) = Rotor(
      letter_roll  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
      position = p,
      notch  = 'W',
      ring   = 'A'
    )
    def type_IV(p: Char) = Rotor(
      letter_roll  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
      position = p,
      notch  = 'K',
      ring   = 'A'
    )
    def type_V(p: Char) = Rotor(
      letter_roll  = "VZBRGITYUPSDNHLXAWMJQOFECK",
      position = p,
      notch  = 'A',
      ring   = 'A'
    )
  }
