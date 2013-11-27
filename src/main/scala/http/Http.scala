package co.torri.res.http

import co.torri.res._
import org.apache.http.impl.nio.client.{CloseableHttpAsyncClient, HttpAsyncClients}
import org.apache.http.client.methods.{HttpGet, HttpUriRequest}
import org.apache.http.HttpResponse
import org.apache.http.concurrent.{Cancellable, FutureCallback}
import concurrent.Promise


class CancelledReq extends Failure("http.cancelled")

class Http private[Http] (http: CloseableHttpAsyncClient) {
  
  @inline
  type HttpRes = (Res[HttpResponse], ReqCancel)
  
  def get(url: String) : HttpRes = execute(new HttpGet(url))

  def execute(req: HttpUriRequest) : HttpRes = {
    val p = Promise[Valid[HttpResponse]]
    
    val reqFuture = http.execute(req, new FutureCallback[HttpResponse] {
      def cancelled() = p.success(Bad(new CancelledReq))
      def completed(result: HttpResponse) = p.success(Good(result))
      def failed(ex: Exception) = p.failure(ex)
    })
    
    val cancellable : ReqCancel = new Cancellable with ReqCancel {
      def cancel(): Boolean = reqFuture.cancel(true)
    }
    
    (Res(p.future), cancellable)
  }
}

object Http {

  def apply() : Http = apply(HttpAsyncClients.createDefault)

  def apply(http: CloseableHttpAsyncClient) : Http = {
    http.start()
    new Http(http)
  }
}

trait ReqCancel { c: Cancellable =>
  def apply() : Unit = cancel()
}