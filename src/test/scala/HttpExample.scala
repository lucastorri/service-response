package co.torri.res.http

import co.torri.res._
import org.apache.http.HttpResponse
import scala.concurrent.duration._
import scala.Some

object HttpExample {

  def main(args: Array[String]) = {

    implicit val waitFor = 10.seconds
    val http = Http()

    val (res, cancel) = http.get("http://example.com")
    // cancel()

    val t : Res[Page] = res
      .map(checkForFailures)
      .map(extractBody)
      .map(asPage)


    t.await match {
      case Good(v) => println(v)
      case Bad(f) => println(f)
      case Err(e) => e.printStackTrace
    }

    res.onComplete {
      case _ => System.exit(0)
    }
  }



  def checkForFailures(hr: HttpResponse) = {
    if (hr.status == 404) throw new NotFound
    hr
  }

  def extractBody(hr: HttpResponse) = {
    hr.body[String]
  }

  def asPage(html: String) = {
    val Title = """<title>([^<]+)</title>""".r
    val Some(Title(title)) = Title.findFirstIn(html: String)
    Page(title)
  }



  class NotFound extends Failure("service.notFound")

  case class Page(title: String)
}
