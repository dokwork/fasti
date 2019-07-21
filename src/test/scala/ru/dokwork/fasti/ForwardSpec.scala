package ru.dokwork.fasti

import cats.implicits._
import org.scalatest.EitherValues._
import org.scalatest.{ FreeSpec, Matchers }
import shapeless.{ :+:, CNil, Coproduct }

import scala.util.Try
import scala.util.control.NoStackTrace

class ForwardSpec extends FreeSpec with Matchers {

  type A = Int
  type B = Long
  type C = Double
  type D = Boolean
  type E = String

  def c = new Coproduct.MkCoproduct[B :+: C :+: D :+: CNil]

  implicit class RichResult(res: Try[(List[B :+: C :+: D :+: CNil], Either[Throwable, E])]) {
    def right = res.get._2.right.value

    def left = res.get._2.left.value

    def list = res.get._1
  }

  val testException = new Exception("Test") with NoStackTrace

  val fab = Forward((x: Int) ⇒ Try(x.toLong))
  val fbc = Forward((x: Long) ⇒ Try(x.toDouble))
  val fcd = Forward((x: Double) ⇒ Try(x > 0.0))
  val fde = Forward((x: Boolean) ⇒ Try(if (x) x.toString else throw testException))

  val forward = fab andThen fbc andThen fcd andThen fde

  "should return list with every next completed stages" in {
    forward(Left(42)).list shouldBe List(c(42L), c(42.0), c(true))
    forward(Right(c(42L))).list shouldBe List(c(42L), c(42.0), c(true))
    forward(Right(c(42.0))).list shouldBe List(c(42.0), c(true))
    forward(Right(c(true))).list shouldBe List(c(true))
  }

  "should return result as right" in {
    forward(Left(42)).right shouldBe "true"
  }

  "should return exception as left" in {
    forward(Left(-42)).left shouldBe testException
  }
}
