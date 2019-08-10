package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import shapeless._

import scala.util.{ Failure, Try }

trait Decode[A, Encoded] {
  def decode(x: Encoded): Try[A]
}

trait DecodeStages[C <: Coproduct, Encoded]{
  def decode(stages: NonEmptyList[Encoded]): Try[NonEmptyList[C]]
}

object DecodeStages {

  implicit def single[H, Encoded](
    implicit
    decoder: Decode[H, Encoded]
  ): DecodeStages[H :+: CNil, Encoded] = new DecodeStages[H :+: CNil, Encoded] {
    type P = H :: HNil

    override def decode(stages: NonEmptyList[Encoded]): Try[NonEmptyList[H :+: CNil]] = stages match {
      case NonEmptyList(x, Nil) => decoder.decode(x).map(h ⇒ NonEmptyList.one(Inl(h)))
      case NonEmptyList(_, t) ⇒ Failure(new IllegalArgumentException(s"Unexpected elements $t."))
    }
  }

  implicit def every[H, TC <: Coproduct, Encoded](
    implicit
    headDecoder: Decode[H, Encoded],
    tailDecoder: DecodeStages[TC, Encoded]
  ): DecodeStages[H :+: TC, Encoded] = new DecodeStages[H :+: TC, Encoded] {

    override def decode(stages: NonEmptyList[Encoded]): Try[NonEmptyList[H :+: TC]] =
      stages match {
        case NonEmptyList(x, Nil) =>
          headDecoder.decode(x).map(h ⇒ NonEmptyList.one(Inl(h)))
        case NonEmptyList(x, t) =>
          for {
            head <- headDecoder.decode(x)
            tail <- tailDecoder.decode(NonEmptyList.fromListUnsafe(t))
          } yield NonEmptyList(Inl(head), tail.map(Inr.apply).toList)
      }
  }
}
