package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import shapeless._

import scala.util.{ Failure, Try }

trait Decode[Enc, Dec] {
  def decode(x: Enc): Try[Dec]
}

trait DecodeStages[Enc, C <: Coproduct]{
  def decode(stages: NonEmptyList[Enc]): Try[NonEmptyList[C]]
}

object DecodeStages {

  implicit def single[Enc, H](
    implicit
    decoder: Decode[Enc, H]
  ): DecodeStages[Enc, H :+: CNil] = new DecodeStages[Enc, H :+: CNil] {
    type P = H :: HNil

    override def decode(stages: NonEmptyList[Enc]): Try[NonEmptyList[H :+: CNil]] = stages match {
      case NonEmptyList(x, Nil) => decoder.decode(x).map(h ⇒ NonEmptyList.one(Inl(h)))
      case NonEmptyList(_, t) ⇒ Failure(new IllegalArgumentException(s"Unexpected elements $t."))
    }
  }

  implicit def every[Enc, H, TC <: Coproduct](
    implicit
    headDecoder: Decode[Enc, H],
    tailDecoder: DecodeStages[Enc, TC]
  ): DecodeStages[Enc, H :+: TC] = new DecodeStages[Enc, H :+: TC] {

    override def decode(stages: NonEmptyList[Enc]): Try[NonEmptyList[H :+: TC]] =
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
