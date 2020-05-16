package enigma

case class Reflector(
                    wiring: String,
                    model: String
                    ){
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def encrypt(charIn: Char): Char = {
    wiring.charAt(alphabet.indexOf(charIn))
  }
}

object Reflectors {
  def UKW_B = Reflector(
    wiring = "YRUHQSLDPXNGOKMIEBFZCWVJAT",
    model = "UKW-B"
  )
  def UKW_C = Reflector(
    wiring = "FVPJIAOYEDRZXWGCTKUQSBNMHL",
    model = "UKW-C"
  )
}