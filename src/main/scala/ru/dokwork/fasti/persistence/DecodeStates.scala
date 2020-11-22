package ru.dokwork.fasti.persistence

import cats.data.NonEmptyList
import ru.dokwork.fasti.BeginFrom
import shapeless._

import scala.util.{ Failure, Try }

trait DecodeStates[S <: HList, E] {
  type P <: HList

  def decode(stages: NonEmptyList[E]): Try[(P, BeginFrom[S, P])]
}

object DecodeStates {

  type Aux[S <: HList, E, P0 <: HList] = DecodeStates[S, E] {type P = P0}

  implicit def build4single[H, E](
    implicit
    decoder: Decode[H, E]
  ): DecodeStates[H :: HNil, E] = new DecodeStates[H :: HNil, E] {
    type P = H :: HNil

    override def decode(stages: NonEmptyList[E]): Try[(P, BeginFrom[H :: HNil, P])] = stages match {
      case NonEmptyList(x, Nil) => decoder.decode(x).map(h => (h :: HNil, BeginFrom.ev4single[H, HNil]))
      case NonEmptyList(_, t) => Failure(new IllegalArgumentException(s"Unexpected elements $t."))
    }
  }

  implicit def build4list[H, TS <: HList, E](
    implicit
    headDecoder: Decode[H, E],
    tailDecoder: DecodeStates[TS, E]
  ): DecodeStates[H :: TS, E] = new DecodeStates[H :: TS, E] {
    type P = H :: tailDecoder.P

    override def decode(stages: NonEmptyList[E]): Try[(P, BeginFrom[H :: TS, P])] =
      stages match {
        case NonEmptyList(_, Nil) =>
          build4single[H, E].decode(stages).asInstanceOf[Try[(P, BeginFrom[H :: TS, P])]]
        case NonEmptyList(x, t) =>
          for {
            head <- headDecoder.decode(x)
            (tail, bf) <- tailDecoder.decode(NonEmptyList.fromListUnsafe(t))
          } yield (head :: tail, BeginFrom.ev4list[H, TS, tailDecoder.P](bf))
      }
  }
}

