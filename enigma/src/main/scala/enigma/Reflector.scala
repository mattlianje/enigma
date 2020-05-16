package enigma

case class Reflector(
                    wiring: String
                    ){
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def encrypt(charIn: Char): Char = {
    wiring.charAt(alphabet.indexOf(charIn))
  }
}

object Reflectors {
  def UKW_B = Reflector(
    wiring = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
  )
  def UKW_C = Reflector(
    wiring = "FVPJIAOYEDRZXWGCTKUQSBNMHL"
  )
}