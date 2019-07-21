package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import shapeless.{ ::, _ }

trait ToStagesList[C <: Coproduct, P <: HList] {
  def toList(p: P): NonEmptyList[C]
}

object ToStagesList {

  def apply[C <: Coproduct, P <: HList](p: P)(implicit instance: ToStagesList[C, P]): NonEmptyList[C] =
    instance.toList(p)

  implicit def single[H, TC <: Coproduct]: ToStagesList[H :+: TC, H :: HNil] =
    (p: H :: HNil) ⇒ NonEmptyList.one(Inl(p.head))

  implicit def every[H, TC <: Coproduct, TP <: HList](
    implicit t: ToStagesList[TC, TP]
  ): ToStagesList[H :+: TC, H :: TP] =
    (p: H :: TP) ⇒ NonEmptyList.one(Inl(p.head)) ::: t.toList(p.tail).map(Inr.apply)
}
