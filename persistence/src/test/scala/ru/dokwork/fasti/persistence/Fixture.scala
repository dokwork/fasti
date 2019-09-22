package ru.dokwork.fasti.persistence

import cats.data.NonEmptyList
import shapeless._

import scala.collection.mutable.ListBuffer
import scala.util.{ Failure, Success, Try }

trait Fixture extends ru.dokwork.fasti.Fixture {

  type Encoded = String

  implicit def encoderB: Encoder[B, Encoded] = (x: B) => x.toString
  implicit def encoderC: Encoder[C, Encoded] = (x: C) => x.toString

  implicit def decoderB: Decoder[B, Encoded] = {
    case "b" ⇒ Success(b)
    case x   ⇒ Failure(new IllegalArgumentException(s"$x"))
  }

  implicit def decoderC: Decoder[C, Encoded] = {
    case "c" ⇒ Success(c)
    case x   ⇒ Failure(new IllegalArgumentException(s"$x"))
  }

  val defaultId: Int = 1

  implicit def idLens[T]: Lens[T, Int] = new Lens[T, Int] {
    override def get(s: T): Int = defaultId

    override def set(s: T)(a: Int): T = s
  }

  implicit val persistence = new TestSagaPersistence()
}

class TestSagaPersistence extends SagaPersistence[Try, String, Int] {
  val storage: ListBuffer[String] = ListBuffer()
  var error: Option[Throwable]    = None
  val completed: ListBuffer[Int]  = ListBuffer()

  override def persist(id: Int, state: String): Try[Unit] = Try {
    storage += state
  }

  override def lastCompensated(id: Int, cause: Throwable): Try[Unit] = Try {
    if (storage.nonEmpty) storage.remove(storage.size - 1)
    error = Some(cause)
  }

  override def completed(id: Int): Try[Unit] = Try {
    completed += id
  }

  override def load(id: Int): Try[(NonEmptyList[String], Option[Throwable])] = Try {
    NonEmptyList.fromListUnsafe(storage.toList) → error
  }
}
