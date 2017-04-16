
import ReaderExample1.ImageExtractorComponent

import scalaz.{Reader, _}
import Scalaz._
import scala.util.Try


object SimpleReaderExample1 extends App {

  val f = Reader((i: Int) ⇒ i * 3)
  val g = Reader((i: Int) ⇒ i * 10)

  val fog =
    for {
      i ← f
      j ← g
    } yield (i,j)

  println(fog.run(10))

}

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

}