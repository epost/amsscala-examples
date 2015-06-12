import scalaz._, Scalaz._
import scalaz.concurrent.{Future, Task}
import scalaz.effect.IO


object ScalazExamplesFromMeetupSession {

  type Err = String
  type Address = String

  // Functor is basically just something you can 'map' over. Map issometimes also called fmap, for 'functorial map'.
  object DefiningOurOwnFunctor {

    trait Functor[F[_]] {
      def fmap[A,B](f: A => B)(xs: F[A]): F[B]
    }

    // provide an instance of the Functor typeclass
    implicit val listFunctorInstance = new Functor[List] {

      // fmap is just another name for map, where f means functorial
      def fmap[A,B](f: A => B)(xs: List[A]) =
        xs map f
    }

    // provide postfix syntax, i.e. `xs.fmap(a)` instead of just `listFunctorInstance.fmap(f)(xs)`
    implicit class FunctorSyntax[F[_], A](fa: F[A])(implicit functor: Functor[F]) {
      def fmap[B](f: A => B): F[B] =
        functor.fmap(f)(fa)
    }

    def twice(x: Int) = 2*x

    List(1,2,3) fmap (_.toString.reverse)
    listFunctorInstance.fmap(twice)(List(1,2,3))
  }


  // Functors are basically structures that you can map over, with some additional restrictions
  // that ensure predictable behaviour, even under refactoring and such
  object FunctorExamplesUsingBuiltinMap {

    Option(42)   map (_.toString.reverse)
    Future(42)   map (_.toString.reverse)
    List(1,2,3)  map (_.toString.reverse)

    // a better way of writing Option(42) is 42.some, since it gets the result type right, i.e. Option[Int] instead of Some[Int]
    42.some       map (1+)

    (1 to 3).toStream.map(2*).toList
    Future(42).map(2*).run

    // Set is NOT a functor since it does not preserve structure; elements
    // 'disappear' under certain functions, such as abs :: Int -> Int:
    Set(1,2,-3,3).map(x => x.abs * x.abs)


    // let's take List as our monad now, instead of Option
    List(1,2,3) map (x => (1 to x).toList)
    (List(1,2,3) map (x => (1 to x).toList)).flatten
  }


  // Monadic structures are functors with the added capability of flattening ('joining')
  // two layers of structure into one. This is done using the method 'flatten', which
  // combines with 'map' to give us 'flatMap', which is also called 'bind' or '>>='.
  //
  // The 'flatten' method requires that we have at least two layers of
  // structure. Therefore, any function we want to flatMap over some monadic
  // value (Option/Future/List/...) must produce *another* layer of
  // Option/Future/List/... structure.
  object MonadExamples {

    type Name   = String
    type Place  = Int
    type Prize  = String
    type Amount = Int

    // let's define some partial functions using Option, and take Option as our example monad
    def prize(place: Place): Option[Prize] = place match {
      case 1 => Some("gold")
      case 2 => Some("silver")
      case 3 => Some("bronze")
      case _ => None
    }

    // only the winner gets something
    def amount(prize: Prize): Option[Amount] = prize match {
      case "gold" => Some(1000)
      case _      => None
    }

    val contestants = Map[Name, Place](
      "Glenn Buterol" -> 1,
      "Waldo"         -> 2,
      "Said"          -> 3,
      "Erik"          -> 4
    )

    contestants get "Glenn Buterol" flatMap prize
    contestants get "Glenn Buterol" >>= prize

    // flatMap is short for 'map, then flatten'; we want to end up with
    // Option(...) instead of Option(Option(...)
    (contestants get "Glenn Buterol" map prize).flatten

    Option(Option(42)).flatten

    contestants get "Glenn Buterol" flatMap prize flatMap amount

    // even shorter: the symbolic alias for flatMap/bind
    contestants get "Glenn Buterol" >>= prize >>= amount

    for {
      thePlace  <- contestants get "Glenn Buterol"
      thePrize  <- prize(thePlace)
      theAmount <- amount(thePrize)
    } yield theAmount

    contestants get "Waldo"         flatMap prize
    contestants get "Waldo"         flatMap prize flatMap amount
    contestants get "Waldo"         >>= prize >>= amount

    contestants get "Erik"          >>= prize >>= amount

    // Example of a List monad
    List(1,2,3) flatMap (x => (1 to x).toList)
  }


  object AddressOptionMonadExample {

    case class Company(name: String, address: Option[Address])
    case class Person(name: String, company: Option[Company])

    val waldo = Person("Waldo", Some(Company("KLM", Some("Schiphol"))))
    val koos = Person("Koos Werkloos", None)

    waldo.company.map(_.address).flatten
    waldo.company.flatMap(_.address).fold("niks")(x=>x)

    // a symbolic alias for bind a.k.a. flatMap
    val a = waldo.company >>= (_.address)

    for {
      company <- waldo.company
      address <- company.address
    } yield address

    for {
      company <- koos.company
      address <- company.address
    } yield address


    val fut1: Future[Err \/ Person] =
      Future(\/-(koos))

    fut1.run
  }


  // Same code, different monad: \/ instead of Option.
  // The \/ is scalaz's Either. It's also called "disjunction", disjoint union, and sometimes "or".
  object AddressDisjunctionMonadExample {

    case class Company(name: String, address: Err \/ Address)
    case class Person(name: String, company:  Err \/ Company)

    val waldo  = Person("Waldo",     Company("KLM",     "Schiphol".right).right)
    val waldo2 = Person("Waldo", \/-(Company("KLM", \/-("Schiphol"))))

    val koos   = Person("Koos Werkloos",     "Hij heeft geen baan".left)
    val koos2  = Person("Koos Werkloos", -\/("Hij heeft geen baan"))

    for {
      company <- koos.company
      address <- company.address
    } yield address
  }


  // ignore for now
  object ComonadExample {

    val l = NonEmptyList(1,2,3)
    l.coflatten

    val s = Stream.range(1, 4)
    s.coflatten.take(2).toList
    s.coflatten.take(3).map(_.toList).toList
  }
}
