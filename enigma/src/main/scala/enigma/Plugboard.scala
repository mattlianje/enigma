package enigma

case class Plugboard(
                    plugboard_params: String
                    ){
  def passThruPlugboard(c:Char): Char = {
    if (plugboard_params contains c.toString) {
      val index = plugboard_params.indexOf(c.toString)
      if (index == plugboard_params.length - 1) {
        val res = plugboard_params.charAt(index - 1)
        res
      }
      else if (index == 0) {
        val res = plugboard_params.charAt(1)
        res
      }
      else if (plugboard_params.charAt(index + 1) == ',') {
        val res = plugboard_params.charAt(index - 1)
        res
      }
      else {
        val res = plugboard_params.charAt(index + 1)
        res
      }
    }
    else {
      c
    }
  }
}

object Plugboards {
  def test_plugboard = Plugboard(
    plugboard_params = "AK,ZC"
  )
}