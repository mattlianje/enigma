package enigma

case class Rotor(
                // letter_map <- Immutable sequence of chars that defines the rotor.
                letter_roll: String,
                // position <- Current char the rotor is at.
                position: Char,
                // notch <- Immutable attribute of the rotor type (i.e. for a II rotor, the notch is always the same)
                notch: Char,
                // ring <- Wiring offset
                ring: Char = 'A',
                // rotor identifier
                model: String
                ){
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

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val ring_int: Int = getPositionOf(alphabet, ring)
  val position_int: Int = getPositionOf(alphabet, position)
  val offset: Int = position_int - ring_int
  val alphabet_size = 26

  /*
   * turnRotor: turns the rotor clockwise relative to machine operator.
   */
  def turnRotor: Rotor = this.copy(
    position = nextLetter(position.toString)
  )

  /*
   * encrypt: encrypted char that comes out of left of rotor after passing char into the right.
   */
  def encrypt(charIn:Char): Char = {
    if (offset == 0) {
      val res = letter_roll.charAt(alphabet.indexOf(charIn))
      res
    }
    else {
      val char_pos_alphabet = alphabet.indexOf(charIn) + 1
      val temp_sum = offset + char_pos_alphabet
      if (temp_sum > alphabet_size) {
        val new_index = (alphabet_size - temp_sum + 1).abs
        val letter_roll_char = letter_roll.charAt(new_index)
        if ((alphabet.indexOf(letter_roll_char) - offset) < 0) {
          val encrypted_char = alphabet.charAt(alphabet_size - offset + alphabet.indexOf(letter_roll_char))
          encrypted_char
        }
        else {
          val encrypted_char = alphabet.charAt(alphabet.indexOf(letter_roll_char) - offset)
          encrypted_char
        }
      }
      else {
        val new_index = char_pos_alphabet + offset - 1
        val letter_roll_char = letter_roll.charAt(new_index)

        if ((alphabet.indexOf(letter_roll_char) - offset) > 0) {
          val encrypted_char = alphabet.charAt(alphabet.indexOf(letter_roll_char) - offset)
          encrypted_char
        }
        else {
          // TODO
          val encrypted_char = alphabet.charAt(alphabet_size + (alphabet.indexOf(letter_roll_char) - offset))
          encrypted_char
        }
      }
    }
  }

  /*
   * reverseEncrypt: like encrypt but pass signal left to right through rotor
   */
  def reverseEncrypt(charIn: Char): Char = {
    if (offset == 0) {
      val res = alphabet.charAt(letter_roll.indexOf(charIn))
      res
    }
    else {
      // TODO
      val offset_char = getOffsetChar(charIn, offset)
      val index_of_offset_char_on_roll = letter_roll.indexOf(offset_char)
      if ((index_of_offset_char_on_roll - offset) < 0) {
        val new_char = alphabet.charAt(alphabet_size - (index_of_offset_char_on_roll - offset).abs)
        new_char
      }
      else {
        val new_char = alphabet.charAt(index_of_offset_char_on_roll - offset)
        new_char
      }
    }
  }

  def nextLetter(x:String) : Char = {
    if (x == "Z") {
      val end = 'A'
      end
    }
    else {
      (x(0) + 1).toChar
    }
  }

  def getOffsetChar(c:Char, offset:Int): Char = {
    if ((alphabet.indexOf(c) + 1 + offset) > alphabet_size) {
      alphabet.charAt((alphabet_size - (alphabet.indexOf(c) + offset)).abs)
    }
    else {
      alphabet.charAt(alphabet.indexOf(c) + offset)
    }
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
      ring   = 'A',
      model = "type I"
    )
    def rotor_II(p: Char) = Rotor(
      letter_roll  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
      position = p,
      notch  = 'F',
      ring   = 'A',
      model = "type II"
    )
    def rotor_III(p: Char) = Rotor(
      letter_roll  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
      position = p,
      notch  = 'W',
      ring   = 'A',
      model = "type III"
    )
    def type_IV(p: Char) = Rotor(
      letter_roll  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
      position = p,
      notch  = 'K',
      ring   = 'A',
      model = "type IV"
    )
    def type_V(p: Char) = Rotor(
      letter_roll  = "VZBRGITYUPSDNHLXAWMJQOFECK",
      position = p,
      notch  = 'A',
      ring   = 'A',
      model = "type V"
    )
  }
