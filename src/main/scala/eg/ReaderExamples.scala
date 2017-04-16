


object ReaderMonadImplementation {

  case class Reader[Conf, T](run: Conf ⇒ T) { self ⇒
    def map[U](f: T ⇒ U): Reader[Conf, U] =
      Reader(self.run andThen f)

    def flatMap[U](f: T ⇒ Reader[Conf, U]): Reader[Conf, U] =
      Reader[Conf, U](conf ⇒ f(self.run(conf)).run(conf))

    def local[Conf1](f: Conf1 ⇒ Conf): Reader[Conf1, T] =
      Reader[Conf1, T](f andThen run)
  }

  object Reader {
    def pure [C, A] (a: A): Reader[C, A] = Reader(_ ⇒ a)

    implicit def funToReader[C, A](read: C ⇒ A): Reader[C, A] =
      Reader(read)
  }
}


object SimpleReaderExample1 extends App {
  import ReaderMonadImplementation._
  val f = Reader((i: Int) ⇒ i * 3)
  val g = Reader((i: Int) ⇒ i * 10)

  val fog =
    for {
      i ← f
      j ← g
    } yield (i,j)

  println(fog.run(10))

}

import scalaz._
import Scalaz._
import scala.util.Try



object ReaderExample1 extends App{

  class Dep1
  class Dep2
  class Dep3

  trait Dep1Component { def dep1: Dep1 }
  trait Dep2Component { def dep2: Dep2 }
  trait Dep3Component { def dep3: Dep3 }

  class PageFetcher {
    def fetch(url: String): Reader[Dep1Component, Try[String]] = Reader((deps: Dep1Component) => Try (url.toLowerCase))
  }

  class ImageExtractor {
    def extractImages(html: String): Reader[Dep2Component with Dep3Component, Try[String]] = Reader ((deps: (Dep2Component with Dep3Component)) ⇒ Try(html) )
  }

  trait PageFetcherComponent { def pageFetcher: PageFetcher }
  trait ImageExtractorComponent { def imageExtractor: ImageExtractor }

  class Dependencies extends PageFetcherComponent
    with ImageExtractorComponent
    with Dep1Component
    with Dep2Component
    with Dep3Component {


    val pageFetcher: PageFetcher = new PageFetcher()
    val imageExtractor: ImageExtractor = new ImageExtractor()
    val dep1: Dep1 = new Dep1()
    val dep2: Dep2 = new Dep2()
    val dep3: Dep3 = new Dep3()
  }

  val dependencies = new Dependencies()

//  def pageFetcherReader = Reader((dep: PageFetcherComponent) ⇒ dep.pageFetcher)
//  def imageExtractorReader = Reader((dep: ImageExtractorComponent) ⇒ dep.imageExtractor)
//  def fetchImage(url: String): Reader[Dependencies, Try[String]] = pageFetcherReader.flatMap((pf: PageFetcher) ⇒ pf.fetch(url))

//
//
//  val url = "TEST"
//
//  val imageFinder = find(url)
//  println(imageFinder.run(dependencies))

}


object ReaderExamples3 extends App {

  case class User(id: Long, email: String)

  trait UserRepository {
    def findAll: List[User]
    def save(user: User): User
  }

  trait DefaultUserRepository extends UserRepository {
    def findAll: List[User] = List(User(123, "pepe@gmail.com"))
    def save(user: User): User = {
      println("Saving user " + user.email)
      user
    }
  }

  trait UserService {
    def findAllReader = Reader((userRepository: UserRepository) => userRepository.findAll)
    def saveReader(user: User) = Reader((userRepository: UserRepository) => userRepository.save(user))
  }

  trait AppConfig extends UserService with DefaultUserRepository

  object Main extends AppConfig {
    def run2(user: User): ReaderT[scalaz.Id.Id, UserRepository, List[User]] = {
      for {
        _      ← saveReader(user)
        users  ← findAllReader
      } yield users
    }
  }

  private val defaultRespositoryConfiguration = new DefaultUserRepository {}
  val result = Main.run2(User(1, "a@a.com")).run(defaultRespositoryConfiguration)

  println(result)

}

object ReaderExample4 extends App {

  case class Book(isbn: String, name: String)

  trait Repository {
    def get(isbn: String): Book
    def getAll: List[Book]
  }

  trait Library {
    import scalaz.Reader

    def getBook(isbn: String) = Reader((repository: Repository) ⇒ repository.get(isbn))

    def getAllBooks = Reader((repository: Repository) ⇒ repository.getAll)
  }

  object LibraryInfo extends Library {
    def bookName(isbn: String) = getBook(isbn) map (_.name)
  }

  import scalaz.Reader

  case class UniversityLibrary(repository: Repository) extends Library {
    def getBookName(isbn: String): String = run(LibraryInfo.bookName(isbn))
    def getAll: String = run(getAllBooks)

    private def run[A](reader: Reader[Repository, A]): String = reader(repository).toString
  }

  object RepositoryImpl extends Repository {
    def get(isbn: String): Book = Book("12323", "test")
    def getAll: List[Book] = List(Book("12323", "test"))
  }
  val universityLibraryApp = UniversityLibrary(RepositoryImpl)

  println(universityLibraryApp.getAll)

}















