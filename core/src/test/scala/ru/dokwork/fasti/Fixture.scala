package ru.dokwork.fasti

import scala.collection.mutable.ListBuffer
import scala.util.control.NoStackTrace
import scala.util.{ Failure, Try }

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

trait Fixture extends Instances with Exceptions {

  val completedStages: ListBuffer[Char] = ListBuffer.empty
  val compensatedStages: ListBuffer[Char] = ListBuffer.empty

  /** Always puts instance of the `Out` to the `completedStages`. */
  def action[In <: Char, Out <: Char : Instance]: In ⇒ Try[Out] = _ ⇒ {
    val b = Instance[Out].get
    completedStages += b
    Try(b)
  }

  /** Always removes instance of the `B` from the `completedStages`. */
  def compensate[T <: Char]: (T, Throwable) ⇒ Try[Unit] = new Function2[T, Throwable, Try[Unit]] {
    override def apply(b: T, e: Throwable): Try[Unit] = Try {
      completedStages -= b
      compensatedStages += b
    }
  }

  /** Always returns `Failure` with `exceptionOnCompensation` */
  def failCompensation[T]: (T, Throwable) => Try[Unit] = (_, _) => Failure(exceptionOnCompensation)

  /** Always returns `Failure` with `testException` */
  def fail[In, Out]: In => Try[Out] = _ => Failure(testException)
}

trait Instance[T] {
  def get: T
}

object Instance {
  def apply[T](implicit instance: Instance[T]): Instance[T] = instance
}

case class TestException(msg: String) extends Exception(msg) with NoStackTrace