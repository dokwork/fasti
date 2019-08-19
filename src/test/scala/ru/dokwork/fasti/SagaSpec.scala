package ru.dokwork.fasti

import cats.implicits._
import org.scalatest.FreeSpec
import org.scalatest.Matchers.{ a ⇒ _, _ }
import org.scalatest.TryValues._
import shapeless.{ :+:, CNil, HNil }

import scala.collection.mutable.ListBuffer
import scala.util.control.NoStackTrace
import scala.util.{ Failure, Try }

class SagaSpec extends FreeSpec {

  import Fixture.all._
  import Saga.Result._

  "Saga" - {
    "should invoke `action` function and return result" in new Fixture {
      // given:
      val saga = Saga(action[A, B])
      // when:
      val result = saga(a)
      // then:
      result.success.value shouldBe Success(b)
      completedStages should contain only b
    }
    "should sequentially invoke actions" in new Fixture {
      // given:
      val saga = Saga(action[A, B]) andThen Saga(action[B, C])
      // when:
      val result = saga(a)
      // then:
      result.success.value shouldBe Success(c)
      completedStages should contain allOf(b, c)
    }

    "continue" - {
      "should continue from the product" in new Fixture {
        // given:
        val saga: Saga[Try, A, C, B :+: CNil] = Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga.continue(b :: HNil)
        // then:
        result.success.value shouldBe Success(c)
        completedStages should contain only c // because we omit first step
      }
    }

    "rollback" - {
      "should return `Rolledback` with a reason" in new Fixture {
        // given:
        val saga = Saga(fail[A, B])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Rolledback(testException)
      }
      "should compensate first step for product" in new Fixture {
        // give:
        val saga = Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga.rollback(b :: HNil, testException)
        // then:
        result.success.value shouldBe Rolledback(testException)
        completedStages shouldBe empty
        compensatedStages should contain(b)
      }
      "should return `RollbackFailed` in the context of `F` when rollback failed" in new Fixture {
        // given:
        val saga = Saga(action[A, B], failRollback[B]) andThen Saga(action[B, C])
        // when:
        val result = saga.rollback(b :: HNil, testException)
        // then:
        result.failure.exception shouldBe an[RollbackFailed[B :+: CNil]]
      }
      "should invoke both compensation actions on rollback" in new Fixture {
        // given:
        val saga =
          Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C]) andThen Saga(fail[C, Any])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Rolledback(testException)
        completedStages shouldBe empty
        compensatedStages should contain allOf(b, c)
      }
      "should do nothing when saga failed on the beginning" in new Fixture {
        // given:
        val saga = Saga(fail[A, B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Rolledback(testException)
        completedStages shouldBe empty
        compensatedStages shouldBe empty
      }
    }
  }
}

trait Fixture {

  import Fixture.exceptions._

  val completedStages: ListBuffer[Char] = ListBuffer.empty
  val compensatedStages: ListBuffer[Char] = ListBuffer.empty

  /** Always puts instance of the `B` to the `completedStages`. */
  def action[A <: Char, B <: Char : Instance]: A ⇒ Try[B] = _ ⇒ {
    val b = Instance[B].get; completedStages += b; Try(b)
  }

  /** Always removes instance of the `B` from the `completedStages`. */
  def compensate[B <: Char]: (B, Throwable) ⇒ Try[Unit] = (b, _) ⇒ Try {
    completedStages -= b
    compensatedStages += b
  }

  /** Always returns `Failure` with `exceptionOnRollback` */
  def failRollback[B]: (B, Throwable) => Try[Unit] = (_, _) => Failure(exceptionOnRollback)

  /** Always returns `Failure` with `testException` */
  def fail[A, B]: A => Try[B] = _ => Failure(testException)
}

object Fixture {

  trait Types {

    trait TagA

    trait TagB

    trait TagC

    trait TagD

    type A = Char with TagA
    type B = Char with TagB
    type C = Char with TagC
    type D = Char with TagD
  }

  trait Instances extends Types {
    implicit val a: A = 'a'.asInstanceOf[A]
    implicit val b: B = 'b'.asInstanceOf[B]
    implicit val c: C = 'c'.asInstanceOf[C]
    implicit val d: D = 'd'.asInstanceOf[D]

    implicit def instance[T](implicit x: T): Instance[T] = new Instance[T] {
      override def get: T = x
    }
  }

  trait Exceptions {
    val exceptionOnRollback = TestException("exceptionOnRollback")
    val testException = TestException("testException")
  }

  object instances extends Instances

  object exceptions extends Exceptions

  object all extends Instances with Exceptions

}

trait Instance[T] {
  def get: T
}

object Instance {
  def apply[T](implicit instance: Instance[T]): Instance[T] = instance
}

case class TestException(msg: String) extends Exception(msg) with NoStackTrace