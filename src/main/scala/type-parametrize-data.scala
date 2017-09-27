/* Based on:
     https://www.youtube.com/watch?v=BHjIl81HgfE

   Java -> you add subclasses to resolve problems or write a singleton
   List -> add a new keyword optional parameter
   Haskell(Scala :-) ) -> you add type parameter
*/
package playground

object web {

  object common {
    type URL = String

    type IO[A] = A
  }

  object case1 {
    import common._

    type Content = String

    case class WebPage(localtion: URL, content: Content, title: Option[String], links: List[(String, URL)])

    def withContent(f: Content => Content)(w: WebPage): WebPage =
      w.copy(content= f(w.content))

    def setContent(c: Content)(w: WebPage): WebPage =
      withContent(_ => c)(w)
  }

  object case2 {
    import common._

    case class WebPage[A](localtion: URL, content: A, title: Option[String], links: List[(String, URL)])

    def withContent[A, B](f: A => B)(w: WebPage[A]): WebPage[B] =
      w.copy(content= f(w.content))

    // This signature indicates that the implementation will NOT use the content of the original WebPage.
    def setContent[A, U](c: A)(w: WebPage[U]): WebPage[A] =
      withContent((_: U) => c)(w)

    // This signature indicates that will NOT look into the content, only retrieving all the link list.
    def fetchLinks[A](w: WebPage[A]): IO[List[WebPage[String]]] =
      ???

    // Now we can use Unit as an empty WebPage... for example, before the page was fetched.
    type Empty = Unit
    val notLoaded: WebPage[Empty] = WebPage("http://my-page", Unit, None, List())
  }

  object case3 {
    import scalaz._
    import Scalaz._

    import case2.{WebPage, Empty}

    implicit object FunctorWebPage extends Functor[WebPage] {
      def map[A, B](fa: WebPage[A])(f: A => B): WebPage[B] =
        fa.copy(content= f(fa.content))
    }

    def withContent[A, B](f: A => B)(w: WebPage[A]): WebPage[B] =
      w.map(f)

    def setContent[A, U](c: A)(w: WebPage[U]): WebPage[A] =
      withContent((_: U) => c)(w)

  }
}

object TypeParametrizeData extends App {

}
