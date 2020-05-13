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
   *  To understand this idea of notch without a picture think of an old-school car mileage odometer.
   *  This is set to our base 10 counting ... where everytime a wheel clicks from 9 to 0 ...
   *  it nudges the wheel to the left by one increment.
   *
   *  https://en.wikipedia.org/wiki/Enigma_rotor_details
   *  Example:
   *    Rotor | Notch  | Effect
   *  _________________________________________________________________________
   *    II    | E      | If rotor steps from E to F, the next rotor is advanced
   *
   *  RING:
   *
   *  All the rotors have ABCDEF ... Z printed on them but can be rotated 360 degrees ...
   *  effectively changing the mapping but not offsets which typify a rotor.
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

  val ring_int: Int = 1 // TODO ...
  val position_int: Int = 2 // TODO
  val offset: Int = position_int - ring_int
  val alphabet_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val alphabet_size = 26

  // TODO (implement object copy)
  // https://medium.com/zyseme-technology/functional-references-lens-and-other-optics-in-scala-e5f7e2fdafe

  def change_pos_test: Rotor = this.copy(
    position = nextLetter(position.toString).charAt(0)
  )

  def nextLetter(s: String) = (s.head + 1).toChar.toString

  /*
  get_position_of: Gets the position of an element in list ...
  @input <- String, Char
  @output -> Int
   */
  def get_position_of(input_string: String, c: Char): Int = {
    val new_list = input_string.toList
    new_list.indexOf(c)
  }

  /*
  get_offset_alphabet: Gets a string of the alphabet starting with the char passed in ...
  @input <- Char
  @output -> List[Char]
  */
  def get_offset_alphabet(c: Char): String = {
    val alphabet: Seq[Char] = ('A' to 'Z').toList
    val position = get_position_of(alphabet_str, c)
    val (left, right) = alphabet.splitAt(position)
    val merged_list = right ++ left
    merged_list.mkString
  }
}
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
