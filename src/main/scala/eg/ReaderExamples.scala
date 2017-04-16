


object ReaderMonadImplementation {

  case class Reader[-C, +A](run: C ⇒ A) { self ⇒

    def apply(c: C): A = run(c)

    def map[B](f: A ⇒ B): Reader[C, B] =
      Reader(c ⇒ f(self.run(c)))

    def flatMap[B, D <: C](f: A ⇒ Reader[D, B]): Reader[D, B] =
      Reader[D, B](conf ⇒ f(self.run(conf)).run(conf))

    def local[D <: C](f: D ⇒ C): Reader[D, A] =
      Reader[D, A](f andThen run)

    def zip[B, D <: C](other: Reader[D, B]): Reader[D, (A, B)] =
      self.flatMap(t ⇒ other.map(b ⇒ (t, b)))

  }

  object Reader {
    def pure [C, A] (a: A): Reader[C, A] = Reader(_ ⇒ a)

    implicit def reader[C, A](read: C ⇒ A): Reader[C, A] =
      Reader(read)

    def sequence[C, R](list: TraversableOnce[Reader[C, R]]): Reader[C, TraversableOnce[R]] = reader { conn =>
      for {
        r <- list
      } yield r(conn)
    }
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


object ReaderExample1 extends App {

  import ReaderMonadImplementation._
  import scala.util.Try

  class Dep1

  class Dep2

  class Dep3

  trait Dep1Component {
    def dep1: Dep1
  }

  trait Dep2Component {
    def dep2: Dep2
  }

  trait Dep3Component {
    def dep3: Dep3
  }

  class PageFetcher {
    def fetch(url: String): Reader[Dep1Component, Try[String]] = Reader((deps: Dep1Component) => Try(url.toLowerCase))
  }

  class ImageExtractor {
    def extractImages(html: String): Reader[Dep2Component with Dep3Component, Try[String]] = Reader((deps: (Dep2Component with Dep3Component)) ⇒ Try(html))
  }

  trait PageFetcherComponent {
    def pageFetcher: PageFetcher
  }

  trait ImageExtractorComponent {
    def imageExtractor: ImageExtractor
  }

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

  def pageFetcherReader = Reader((dep: PageFetcherComponent) ⇒ dep.pageFetcher)

  def imageExtractorReader = Reader((dep: ImageExtractorComponent) ⇒ dep.imageExtractor)

  def fetchImage(url: String): Reader[Dependencies, Try[String]] = pageFetcherReader.flatMap((pf: PageFetcher) ⇒ pf.fetch(url))


  def find(url: String) = {
    for {
      pageFetcher ← pageFetcherReader
      imageExtractor ← imageExtractorReader
      htmlTry ← pageFetcher.fetch(url)
      images ← imageExtractor.extractImages(htmlTry.get)
    } yield images
  }


  val url = "TEST"
  val imageFinder = find(url)
  println(imageFinder.run(dependencies).get)
}


object ReaderExamples3 extends App {
  import scalaz._, Scalaz._

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
  import scalaz._, Scalaz._
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
