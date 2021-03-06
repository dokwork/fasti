package ru.dokwork.fasti

import cats.implicits._
import shapeless._
import scala.util._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ForwardSpec extends AnyFreeSpec with Matchers {

  import Forward._

  type A = Int
  type B = Long
  type C = Double
  type D = Boolean
  type E = String

  val fab: Forward[Try, A, B, HNil] = Forward((x: A) => Try(x.toLong))
  val fbc: Forward[Try, B, C, HNil] = Forward((x: B) => Try(x.toDouble))
  val fac: Forward[Try, A, C, B :: HNil] = fab andThen fbc
  val fcd: Forward[Try, C, D, HNil] = Forward((x: C) => Try(x > 0))
  val fde: Forward[Try, D, E, HNil] = Forward((x: D) => Try(x.toString))
  val fce: Forward[Try, C, E, D :: HNil] = fcd andThen fde
  val fae: Forward[Try, A, E, B :: C :: D :: HNil] = fac andThen fce

  "Forward" - {
    "on success" - {
      val f = fae
      "should successfully return expected result" in {
        f(1) shouldBe Success(Right("true"))
      }
      "should return the same result as on apply" in {
        f.continue(1L :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: true :: HNil) shouldBe f(1)
      }
    }
    "on fail" - {
      val testException = new Exception("Test")
      val f = fae andThen Forward[Try, E, Any](_ => Failure(testException))
      "should return list of the completed states with exception" in {
        f(1) shouldBe Success(Left(("true" :: true :: 1.0 :: 1L :: HNil) -> testException))
      }
      "should return the same result as on apply" in {
        f.continue(1L :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: true :: HNil) shouldBe f(1)
        f.continue(1L :: 1.0 :: true :: "true" :: HNil) shouldBe f(1)
      }
    }
  }
}
