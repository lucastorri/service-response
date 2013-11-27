package co.torri.res

import scala.util._
import concurrent.{Await, Promise, Future, ExecutionContext}
import ExecutionContext.Implicits.global
import concurrent.duration.Duration

trait NoStackTrace { e: Exception =>
  override def fillInStackTrace(): Throwable = this
}

class Failure(val label: String, message: String) extends Exception(message) with NoStackTrace {
  def this(label: String) = this(label, null)
}

class FilterFailure extends Failure("filter.notSatisfied")


sealed trait Result[+R]
sealed trait Valid[+R] extends Result[R]
sealed trait Invalid[+R] extends Result[R]
case class Good[+G](v: G) extends Valid[G]
case class Bad(f: Failure) extends Valid[Nothing]
case class Error(e: Throwable) extends Invalid[Nothing]

case class Res[R](future: Future[Valid[R]]) {

  def onComplete(f: PartialFunction[Result[R], Any]) : Unit = transform(f)

  def map[RR](f: R => RR) : Res[RR] = fwd { (v, p) =>
    p.success(Good(f(v)))
  }

  def flatMap[RR](f: R => Res[RR]) : Res[RR] = fwd { (v, p) =>
    p.completeWith(f(v).future)
  }

  def foreach(f: R => Unit) : Unit =  onComplete {
    case Good(v) => f(v)
  }

  def filter(f: R => Boolean) : Res[R] = fwd { (v, p) =>
    if (f(v)) p.success(Good(v)) else throw new FilterFailure
  }

  def transform[RR](t: PartialFunction[Result[R], RR]) : Res[RR] = {
    val p = Promise[Valid[RR]]()
    future.onComplete {
      case Success(Good(v)) => handle(p) { p.success(Good(t(Good(v)))) }
      case Success(Bad(f)) => handle(p) { p.success(Good(t(Bad(f)))) }
      case Failure(e) => handle(p) { p.success(Good(t(Error(e)))) }
    }
    Res(p.future)
  }

  def zip[OR](other: Res[OR]) : Res[(R, OR)] = {
    val p = Promise[Valid[(R, OR)]]()
    onComplete {
      case Good(v) => other.onComplete {
        case Good(ov) => p.success(Good(v, ov))
        case Bad(f) => p.success(Bad(f))
        case Error(e) => p.failure(e)
      }
      case Bad(f) => p.success(Bad(f))
      case Error(e) => p.failure(e)
    }
    Res(p.future)
  }

  def isCompleted : Boolean = future.isCompleted

  def value : Option[Result[R]] = {
    future.value.map {
      case Success(Good(v)) => Good(v)
      case Success(Bad(f)) => Bad(f)
      case Failure(t) => Error(t)
    }
  }

  def rescueWith[RR >: R](rs: => Res[RR]) : Res[RR] = {
    val p = Promise[Valid[RR]]()
    onComplete {
      case Good(v) => p.success(Good(v))
      case Bad(f) => handle(p) { p.completeWith(rs.future) }
      case Error(e) => p.failure(e)
    }
    Res(p.future)
  }

  def fallbackTo[RR >: R](fb: => Res[RR]) : Res[RR] = {
    val p = Promise[Valid[RR]]()
    onComplete {
      case Good(v) => p.success(Good(v))
      case _ => handle(p) { p.completeWith(fb.future) }
    }
    Res(p.future)
  }

  @inline
  private[this] def handle[RR](p: Promise[Valid[RR]])(f: => Unit) =
    try { f }
    catch {
      case f: Failure => p.success(Bad(f))
      case e: Throwable => p.failure(e)
    }

  @inline
  private[this] def fwd[RR](f: (R, Promise[Valid[RR]]) => Unit) : Res[RR] = {
    val p = Promise[Valid[RR]]()
    onComplete {
      case Good(v) => handle(p) { f(v, p) }
      case Bad(f) => p.success(Bad(f))
      case Error(e) => p.failure(e)
    }
    Res(p.future)
  }

}

object Res {

  def apply[R](v: => Valid[R]) : Res[R] = new Res(Future {
    try { v } catch { case f: Failure => Bad(f) }
  })

  def result[R](r: Result[R]) : Res[R] = r match {
    case Good(v) => good(v)
    case Bad(f) => bad(f)
    case Error(e) => error(e)
  }

  def good[R](v: R) : Res[R] = new Res(Future(Good(v)))

  def bad[R](f: Failure) : Res[R] = new Res(Future(Bad(f)))

  def error[R](e: Throwable) : Res[R] = new Res(Future.failed(e))

  def await[R](r: Res[R])(implicit atMost: Duration) : Result[R] =
    try Await.result(r.future, atMost) catch { case e: Throwable => Error(e) }

}