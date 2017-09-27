package playground

// https://github.com/kenbot/goggles
object GogglesPlay extends App {
  // First example about what are we talking about

  import goggles._

  case class Topping(cherries: Int)
  case class Cake(toppings: List[Topping])
  case class Bakery(cakes: List[Cake])

  val myBakery = Bakery(
    List(Cake(List(Topping(0), Topping(3))), Cake(List(Topping(4))), Cake(Nil)))

  get"$myBakery.cakes*.toppings[0].cherries"

  set"$myBakery.cakes*.toppings[0].cherries" := 7

  // Interpolate any Monocle optic

  import goggles._
  import monocle.std.string.stringToInt
  import monocle.std.int.intToChar

  get"${"113"}.$stringToInt.$intToChar"
  set"${"113"}.$stringToInt.$intToChar" ~= (_.toUpper)

  // Compose Monocle optics

  import goggles._
  import monocle.std.string.stringToInt
  import monocle.std.int.intToChar

  val myLens = lens"$stringToInt.$intToChar"

  myLens.getOption("113")

  // Traverse over collections

  import goggles._

  case class Point(x: Double, y: Double)
  val polygon =
    List(Point(0.0, 0.0), Point(0.0, 1.0), Point(1.0, 1.0), Point(1.0, 0.0))

  get"$polygon*.x"
  set"$polygon*.x" += 1.5

  // Select optional values

  import goggles._

  case class Estate(farm: Option[Farm])
  case class Farm(prizeChicken: Option[Chicken])
  case class Chicken(egg: Option[Egg])
  case class Egg(weight: Double)
  val estate = Estate(Some(Farm(Some(Chicken(Some(Egg(2.3)))))))

  get"$estate.farm?.prizeChicken?.egg?.weight"
  set"$estate.farm?.prizeChicken?.egg?.weight" *= 2

  // Select indexed values

  import goggles._

  sealed trait Square
  object Square {
    case object - extends Square
    case object X extends Square
    case object O extends Square
  }

  val i = 0
  val ticTac = Vector(Vector(Square.X, Square.-, Square.-),
    Vector(Square.O, Square.X, Square.-),
    Vector(Square.-, Square.O, Square.O))

  get"$ticTac[$i][0]"
  set"$ticTac[2][0]" := Square.O

}
