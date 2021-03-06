package co.torri.res

import org.scalatest.{Matchers, FlatSpec}
import scala.concurrent.duration._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ResTest extends FlatSpec with Matchers {

  import Res.await

  implicit val waitFor = 1.second

  val aValue = 10
  val failure = new Failure("some.failure")
  val exception = new Exception

  val good : Res[Int] = Res.good(aValue)
  val bad : Res[Int] = Res.bad(failure)
  val error : Res[Int] = Res.error(exception)

  "Res" must "map" in {
    await(good.map(_ + 10)) should be (Good(aValue + 10))
    await(bad.map(_ + 10)) should be (Bad(failure))
    await(error.map(_ + 10)) should be (Err(exception))
  }

  it must "catch exception on map" in {
    val otherException = new Exception
    await(good.map(_ => throw otherException)) should be (Err(otherException))
  }

  it must "flatMap" in {
    await(good.flatMap(_ => Res.good(15))) should be (Good(15))
    await(good.flatMap(_ => bad)) should be (Bad(failure))
    await(good.flatMap(_ => error)) should be (Err(exception))

    await(bad.flatMap(_ => good)) should be (Bad(failure))
    await(error.flatMap(_ => good)) should be (Err(exception))
  }

  it must "catch exception on flatMap" in {
    val otherException = new Exception
    await(good.flatMap(_ => throw otherException)) should be (Err(otherException))
  }

  it must "filter" in {
    await(good.filter(_ == aValue)) should be (Good(aValue))
    val Bad(e) = await(good.filter(_ != aValue))
    e.isInstanceOf[FilterFailure] should be (true)

    await(bad.filter(_ == 10)) should be (Bad(failure))
    await(error.filter(_ == 10)) should be (Err(exception))
  }

  it must "catch exception on filter" in {
    val otherException = new Exception
    await(good.filter(_ => throw otherException)) should be (Err(otherException))
  }

  it must "foreach" in {
    var goodCalled = false
    var otherCalled = false

    good.foreach(_ => goodCalled = true)
    bad.foreach(_ => otherCalled = true)
    error.foreach(_ => otherCalled = true)

    goodCalled should be (true)
    otherCalled should be (false)
  }

  it must "transform" in {
    val t : PartialFunction[Result[Int], Int] = {
      case Good(v) => 1
      case Bad(f) => 2
      case Err(e) => 3
    }

    await(good.transform(t)) should be (Good(1))
    await(bad.transform(t)) should be (Good(2))
    await(error.transform(t)) should be (Good(3))
  }

  it must "catch exception on transform" in {
    val otherException = new Exception
    await(good.transform { case Good(v) => throw otherException }) should be (Err(otherException))
  }

  it must "zip" in {
    await(good.zip(good)) should be (Good(aValue, aValue))

    await(good.zip(bad)) should be (Bad(failure))
    await(bad.zip(good)) should be (Bad(failure))

    await(good.zip(error)) should be (Err(exception))
    await(error.zip(good)) should be (Err(exception))

    await(bad.zip(error)) should be (Bad(failure))
    await(error.zip(bad)) should be (Err(exception))
  }

  it must "return value" in {
    val res = Res.good {
      Thread.sleep(500)
      10
    }

    res.value should be (None)
    await(res)
    res.value should be (Some(Good(10)))
  }

  it must "rescue" in {
    val rs = Res.good(5)

    await(good.rescueWith(rs)) should be (Good(aValue))
    await(bad.rescueWith(rs)) should be (Good(5))
    await(error.rescueWith(rs)) should be (Err(exception))
  }

  it must "fallback" in {
    val fb = Res.good(5)

    await(good.fallbackTo(fb)) should be (Good(aValue))
    await(bad.fallbackTo(fb)) should be (Good(5))
    await(error.fallbackTo(fb)) should be (Good(5))
  }

  it must "turn thrown Failures as Bad" in {
    await(good.map { _ => throw failure }) should be (Bad(failure))

    def body : Valid[Int] = throw failure
    await(Res(body)) should be (Bad(failure))
  }

  it must "build correctly" in {
    def fail : Int = throw failure
    def err : Int = throw exception

    await(Res(7)) should be (Good(7))
    await(Res(fail)) should be (Bad(failure))
    await(Res(err)) should be (Err(exception))
  }

  it must "work with for comprehension" in {
    val newGood =
      for {
        g1 <- good
        if g1 == aValue
        g2 <- Res.good(7)
      } yield g1 + g2

    val newBad =
      for {
        g1 <- good
        g2 <- bad
      } yield g1 + g2

    val newErr =
      for {
        g1 <- good
        g2 <- error
      } yield g1 + g2

    val badFirst =
      for {
        g1 <- bad
        g2 <- error
      } yield g1 + g2

    val errFirst = for {
      g1 <- error
      g2 <- bad
    } yield g1 + g2

    val filtered = for {
      g <- Res.good(-1)
      if g > 0
    } yield g

    await(newGood) should be (Good(aValue + 7))
    await(newBad) should be (Bad(failure))
    await(newErr) should be (Err(exception))

    await(badFirst) should be (Bad(failure))
    await(errFirst) should be (Err(exception))
    val Bad(e) = await(filtered)
    e.isInstanceOf[FilterFailure] should be (true)
  }

}
