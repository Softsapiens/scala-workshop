package playground

//import scala.language.higherKinds
//import scala.language.reflectiveCalls

object StackPlay extends App {
  sealed trait StackDsl {
    type T
    type Stack[_]

    def push(x: T, s: Stack[T]): Stack[T]
    def top(s: Stack[T]): T
    def pop(s: Stack[T]): Stack[T]
  }

  val intStack = new StackDsl {
    type T = Int
    type Stack[A] = Array[A]

    def empty: Stack[T] = Array[T]()
    def push(x: T, s: Stack[T]): Stack[T] = x +: s
    def top(s: Stack[T]): T = s(0)
    def pop(s: Stack[T]): Stack[T] = ???
  }

  val strStack = new StackDsl {
    type T = String
    type Stack[A] = Array[A]

    def empty: Stack[T] = Array[T]()
    def push(x: T, s: Stack[T]): Stack[T] = x +: s
    def top(s: Stack[T]): T = s(0)
    def pop(s: Stack[T]): Stack[T] = ???
  }

  println(s"""${intStack.top(intStack.push(2, intStack.push(1, intStack.empty)))}""")

  val s = intStack.push(1, intStack.empty)
  //intStack.push(1, strStack.empty)

  sealed trait StackDsl2[T] {
    type Stack

    def push(x: T, s: Stack): Stack
    def top(s: Stack): T
    def pop(s: Stack): Stack
  }

  val intStack2 = new StackDsl2[Int] {
    type T = Int
    type Stack = Array[T]

    def empty: Stack = Array[T]()
    def push(x: T, s: Stack): Stack = x +: s
    def top(s: Stack): T = s(0)
    def pop(s: Stack): Stack = ???
  }

  val strStack2 = new StackDsl2[String] {
    type T = String
    type Stack = Array[T]

    def empty: Stack = Array[T]()
    def push(x: T, s: Stack): Stack = x +: s
    def top(s: Stack): T = s(0)
    def pop(s: Stack): Stack = ???
  }

  println(s"""${intStack2.top(intStack2.push(1, intStack2.empty))}""")

  val s2 = intStack2.push(1, intStack2.empty)
  //intStack2.push(1, strStack2.empty)

  import scala.math.Numeric

  sealed trait StackDsl_[T] {
    val num: Numeric[T]
    type Stack

    def push(x: T, s: Stack): Stack
    def top(s: Stack): T
    def pop(s: Stack): Stack
  }

  val intStack_ = new StackDsl_[Int] {
    type T = Int
    type Stack = Array[T]
    val num: Numeric[T] = implicitly[Numeric[T]]

    def empty: Stack = Array(num.zero)
    def push(x: T, s: Stack): Stack = x +: s
    def top(s: Stack): T = s(0)
    def pop(s: Stack): Stack = ???
  }

  // val strStack_ = new StackDsl_[String] {
  //   type T = String
  //   type Stack = Array[T]
  //   val num: Numeric[T] = implicitly[Numeric[T]]    <---  Not Exists by default

  //   def empty: Stack = Array[T]()
  //   def push(x: T, s: Stack): Stack = x +: s
  //   def top(s: Stack): T = s(0)
  //   def pop(s: Stack): Stack = ???
  // }
}
