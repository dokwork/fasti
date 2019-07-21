package ru.dokwork.fasti

import cats.data.NonEmptyList
import cats.implicits._
import org.scalatest.TryValues._
import org.scalatest.{ EitherValues, FreeSpec, Matchers }
import ru.dokwork.fasti.shapeless_ops.Decode
import shapeless.{ :+:, CNil, HNil }

import scala.language.postfixOps
import scala.util.{ Failure, Try }

class SagaSpec extends FreeSpec with Matchers with EitherValues {

  import Saga.Result._

  "Saga" - {
    "should invoke `action` function and return result" in new Fixture {
      // when:
      val result = Saga(add).apply(5.0).get

      // then:
      result shouldBe Success("5.0")
      counter shouldBe 5.0
    }
    "should sequentially invoke actions" in new Fixture {
      // when:
      val result = Saga(add) andThen Saga(put) apply 5.0 get

      // then:
      counter shouldBe 5.0
      buffer shouldBe Map(0 → "5.0")
      result shouldBe Success(0)
    }

    "continue" - {
      type A = Double
      type B = String
      type C = Int
      type Stages = B :+: CNil

      "should continue from the product" in new Fixture {
        val saga: Saga[Try, A, C, Stages] = Saga(add, compensateAdd) andThen Saga(put, compensatePut)

        // when:
        val result = saga.continue("5.0" :: HNil).get

        // then:
        result shouldBe Success(0)
        buffer shouldBe Map(0 -> "5.0")
        counter shouldBe 0.0 // because we omit first step
      }

      "should continue from the list" in new Fixture {
        implicit val strDecode: Decode[String, String] = (x: String) => Try(x)

        val saga: Saga[Try, A, C, Stages] = Saga(add, compensateAdd) andThen Saga(put, compensatePut)

        // when:
        val result = saga.continue(NonEmptyList.of("5.0")).get

        // then:
        result shouldBe Success(0)
        buffer shouldBe Map(0 -> "5.0")
        counter shouldBe 0.0 // because we omit first step
      }
      "should return exception in the context of F" in new Fixture {
        implicit val strDecode: Decode[String, String] = _ => Failure(testException)

        val saga: Saga[Try, A, C, Stages] = Saga(add, compensateAdd) andThen Saga(put, compensatePut)

        // when:
        val result = saga.continue(NonEmptyList.of("5.0"))

        // then:
        result.failure.exception shouldBe testException
      }
    }

    "rollback" - {
      "should return `Rolledback` with reason" in new Fixture {
        // given:
        val saga = Saga(fail[Double, Any])
        // when:
        val result = saga.apply(5.0)
        // then:
        result.get shouldBe Rolledback(testException)
      }
      "should compensate first step for product" in new Fixture {
        // give:
        val saga = Saga(add, compensateAdd) andThen Saga(put, compensatePut)
        // when:
        val result = saga.rollback("5.0" :: HNil, testException)
        // then:
        result.get shouldBe Rolledback(testException)
        counter shouldBe -5.0
      }
      "should compensate first step for list" in new Fixture {
        // give:
        implicit val strDecode: Decode[String, String] = (x: String) => Try(x)
        val saga = Saga(add, compensateAdd) andThen Saga(put, compensatePut)
        // when:
        val result = saga.rollback(NonEmptyList.of("5.0"), testException)
        // then:
        result.get shouldBe Rolledback(testException)
        counter shouldBe -5.0
      }
      "should return exception in the context of `F` when decode failed" in new Fixture {
        // give:
        implicit val strDecode: Decode[String, String] = _ => Failure(testException)
        val saga = Saga(add, compensateAdd) andThen Saga(put, compensatePut)
        // when:
        val result = saga.rollback(NonEmptyList.of("5.0"), exceptionOnRollback)
        // then:
        result.failure.exception shouldBe testException
      }
      "should return `UnhandledException` in the context of `F` when rollback failed" in new Fixture {
        // given:
        val saga = Saga(add, failRollback[String]) andThen Saga(put)
        // when:
        val result = saga.rollback("5" :: HNil, testException)
        // then:
        result.failure.exception shouldBe an[UnhandledException[String :+: CNil]]
      }
      "should invoke both compensation actions on rollback" in new Fixture {
        // given:
        val saga: Saga[Try, Double, Any, String :+: Int :+: CNil] =
          Saga(add, compensateAdd) andThen Saga(put, compensatePut) andThen Saga(fail[Int, Any])
        // when:
        val result = saga.apply(5.0)
        // then:
        result.get shouldBe Rolledback(testException)
        buffer shouldBe empty
        counter shouldBe 0.0
      }
      "should do nothing when saga failed on the beginning" in new Fixture {
        // given:
        val saga = Saga(fail[Double, String]) andThen Saga(put, compensatePut)
        buffer += 0 -> "0.0"
        // when:
        val result = saga.apply(5.0)
        // then:
        result.get shouldBe Rolledback(testException)
        buffer shouldBe Map(0 -> "0.0")
      }
    }
  }

  class Fixture extends Counter with Buffer with Issue
}
