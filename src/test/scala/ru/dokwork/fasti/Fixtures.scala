package ru.dokwork.fasti

import scala.collection.mutable
import scala.util.{ Failure, Try }
import scala.util.control.NoStackTrace

trait Counter {
  var counter = 0.0
  /** Adds value to the global mutable `counter` and return this value as `String` */
  def add: Double => Try[String] =
    value =>
      Try {
        counter += value
        counter.toString
    }
  /** Converts string to double and remove it from the global mutable counter */
  def compensateAdd: (String, Throwable) => Try[Unit] =
    (value, _) => Try { counter -= value.toDouble }
}

trait Buffer {
  val buffer = mutable.Map[Int, String]()
  /** Puts string value to the global mutable `buffer` and return a number of the added value (begin from 0) */
  def put: String => Try[Int] =
    value =>
      Try {
        buffer += buffer.size → value
        buffer.size - 1
    }
  /** Removes record with specified number from the global mutable `buffer` */
  def compensatePut: (Int, Throwable) => Try[Unit] =
    (key, _) => Try { buffer -= key }
}

trait Issue {
  val exceptionOnRollback                 = new Exception("exceptionOnRollback") with NoStackTrace
  /** Returns `Failure` */
  def failRollback[B]: (B, Throwable) => Try[Unit] = (_, _) => Failure(exceptionOnRollback)

  val testException                       = new Exception("testException") with NoStackTrace
  /** Returns `Failure` with `testException` inside for the any argument */
  def fail[A, B]: A => Try[B]             = _ => Failure(testException)
}
