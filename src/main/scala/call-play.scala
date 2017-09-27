package playground

object CallPlay extends App {
  // call-by-value
  // Evaluates before calling the function
  def fooValue(cond: Boolean)(bar: String) = {
    Thread.sleep(300)
    println(s"begin fooValue at ${System.currentTimeMillis}")
    if (cond) bar + { Thread.sleep(300); bar } else ""
  }

  println(fooValue(true) { s" ts ${System.currentTimeMillis} " })

  // call-by-name
  // IMPORTANT: Evaluates bar each time it is used
  def fooName(cond: Boolean)(bar: => String) = {
    Thread.sleep(300)
    println(s"begin fooName at ${System.currentTimeMillis}")
    if (cond) bar + { Thread.sleep(300); bar } else ""
  }

  println(fooName(true) { s" ts ${System.currentTimeMillis} " })

  // call-by-need: Option1
  def fooNeed1(cond: Boolean)(bar: => String) = {
    Thread.sleep(300)
    println(s"begin fooNeed1 at ${System.currentTimeMillis}")
    lazy val lazyBar = bar
    if (cond) lazyBar + { Thread.sleep(300); lazyBar } else ""
  }

  println(fooNeed1(true) { s" ts ${System.currentTimeMillis} " })

  // call-by-need: Option2
  def fooNeed2(cond: Boolean)(bar: => String) = {
    Thread.sleep(300)
    println(s"begin fooNeed2 at ${System.currentTimeMillis}")
    if (cond) bar + { Thread.sleep(300); bar } else ""
  }

  lazy val bar = s" ts ${System.currentTimeMillis} "

  println(fooNeed2(true)(bar))
}
