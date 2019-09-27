package ru.dokwork.fasti.persistence

import cats.data.NonEmptyList
import ru.dokwork.fasti.BeginFrom
import shapeless._

import scala.util.{ Failure, Try }

trait DecodeStates[S <: HList, Encoded] {
  type P <: HList

  def decode(stages: NonEmptyList[Encoded]): Try[(P, BeginFrom[S, P])]
}

object DecodeStates {

  type Aux[S <: HList, Encoded, P0 <: HList] = DecodeStates[S, Encoded] {type P = P0}

  implicit def build4single[H, Encoded](
    implicit
    decoder: Decoder[H, Encoded]
  ): DecodeStates[H :: HNil, Encoded] = new DecodeStates[H :: HNil, Encoded] {
    type P = H :: HNil

    override def decode(stages: NonEmptyList[Encoded]): Try[(P, BeginFrom[H :: HNil, P])] = stages match {
      case NonEmptyList(x, Nil) => decoder.decode(x).map(h => (h :: HNil, BeginFrom.ev4single[H, HNil]))
      case NonEmptyList(_, t) => Failure(new IllegalArgumentException(s"Unexpected elements $t."))
    }
  }

  implicit def build4list[H, TS <: HList, Encoded](
    implicit
    headDecoder: Decoder[H, Encoded],
    tailDecoder: DecodeStates[TS, Encoded]
  ): DecodeStates[H :: TS, Encoded] = new DecodeStates[H :: TS, Encoded] {
    type P = H :: tailDecoder.P

    override def decode(stages: NonEmptyList[Encoded]): Try[(P, BeginFrom[H :: TS, P])] =
      stages match {
        case NonEmptyList(_, Nil) =>
          build4single[H, Encoded].decode(stages).asInstanceOf[Try[(P, BeginFrom[H :: TS, P])]]
        case NonEmptyList(x, t) =>
          for {
            head <- headDecoder.decode(x)
            (tail, bf) <- tailDecoder.decode(NonEmptyList.fromListUnsafe(t))
          } yield (head :: tail, BeginFrom.ev4list[H, TS, tailDecoder.P](bf))
      }
  }
}
