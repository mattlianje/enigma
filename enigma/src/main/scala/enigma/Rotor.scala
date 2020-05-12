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
   *  Rotor I in position A-1:
   *  EKMFLGDQVZNTOWYHXUSPAIBRCJ
   *  abcdefghijklmnopqrstuvwxyz
   *
   *  Rotor I in position B-2:
   *  EKMFLGDQVZNTOWYHXUSPAIBRCJ
   *  zabcdefghijklmnopqrstuvwxy
   *
   */

  val ringAsInt: Int = ring + 'A'
  val posistionAsInt: Int = 'A' + position
  val offset: Int = posistionAsInt - ringAsInt
  val alphabet_size = 26

  def get_char(c: Char, w: Seq[Char]): Char = {
    val adjustment = adjust(c)
    val resultOffset = (alphabet_size + w(adjustment) - 'A' - offset) % alphabet_size
    val output = ('A' + resultOffset).toChar
    output
  }

  private def adjust(c: Char): Int =
    (alphabet_size + (c - 'A') + offset) % alphabet_size
}
  object Rotors {
    def type_I(p: Char) = Rotor(
      letter_roll  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
      position = p,
      notch  = 'R',
      ring   = 'A'
    )
    def type_II(p: Char) = Rotor(
      letter_roll  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
      position = p,
      notch  = 'F',
      ring   = 'A'
    )
    def type_III(p: Char) = Rotor(
      letter_roll  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
      notch  = 'W',
      ring   = 'A',
      position = p
    )
    def type_IV(p: Char) = Rotor(
      letter_roll  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
      position = p,
      notch  = 'K',
      ring   = 'A',
    )
    def type_V(p: Char) = Rotor(
      letter_roll  = "VZBRGITYUPSDNHLXAWMJQOFECK",
      notch  = 'A',
      ring   = 'A',
      position = p
    )
}
