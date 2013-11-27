package co.torri.res

import org.apache.http.{HttpEntity, HttpResponse}
import io.Source
import xml.{XML, Node}

package object http {

  type BodyConverter[T] = (HttpEntity) => T

  implicit val ent2str : BodyConverter[String] = e => Source.fromInputStream(e.getContent, "utf-8").mkString
  implicit val ent2xml : BodyConverter[Node] = e => XML.load(e.getContent)

  implicit class HttpResponseExtra(res: HttpResponse) {

    def status : Int = res.getStatusLine.getStatusCode

    def headers: Map[String, String] = res.getAllHeaders.map { h =>
      (h.getName, h.getValue)
    }.toMap

    def body[T : BodyConverter] : T = {
      val converter = implicitly[BodyConverter[T]]
      converter(res.getEntity)
    }
  }

}
