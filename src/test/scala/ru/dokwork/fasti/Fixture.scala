package ru.dokwork.fasti

import scala.collection.mutable.ListBuffer
import scala.util.control.NoStackTrace
import scala.util.{ Failure, Try }

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
    val exceptionOnCompensation = TestException("exceptionOnCompensation")
    val testException = TestException("testException")
  }

  object instances extends Instances

  object exceptions extends Exceptions

  object all extends Instances with Exceptions

}

trait Fixture {

  import Fixture.exceptions._

  val completedStages: ListBuffer[Char] = ListBuffer.empty
  val compensatedStages: ListBuffer[Char] = ListBuffer.empty

  /** Always puts instance of the `B` to the `completedStages`. */
  def action[A <: Char, B <: Char : Instance]: A ⇒ Try[B] = _ ⇒ {
    val b = Instance[B].get
    completedStages += b
    Try(b)
  }

  /** Always removes instance of the `B` from the `completedStages`. */
  def compensate[B <: Char]: (B, Throwable) ⇒ Try[Unit] = new Function2[B, Throwable, Try[Unit]] {
    override def apply(b: B, e: Throwable): Try[Unit] = Try {
      completedStages -= b
      compensatedStages += b
    }
  }

  /** Always returns `Failure` with `exceptionOnCompensation` */
  def failCompensation[B]: (B, Throwable) => Try[Unit] = (_, _) => Failure(exceptionOnCompensation)

  /** Always returns `Failure` with `testException` */
  def fail[A, B]: A => Try[B] = _ => Failure(testException)
}

trait Instance[T] {
  def get: T
}

object Instance {
  def apply[T](implicit instance: Instance[T]): Instance[T] = instance
}

case class TestException(msg: String) extends Exception(msg) with NoStackTrace