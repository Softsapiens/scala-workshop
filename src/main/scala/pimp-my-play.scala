package playground

object PimpPlay extends App {
  class BlingString(string: String) {
    def bling = "*" + string + "*"
  }

  implicit def blingYoString(string: String) = new BlingString(string)

  "hi".bling

  // In order to avoid overhead for anonymous class creation
  implicit class BlingString2(string: String) {
    def bling2 = "*" + string + "*"
  }

  "hi all".bling2
}
