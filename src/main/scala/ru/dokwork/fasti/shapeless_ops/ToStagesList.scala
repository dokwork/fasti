package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import shapeless.{ ::, _ }

trait ToStagesList[C <: Coproduct, P <: HList] {
  def toList(p: P): NonEmptyList[C]
}

object ToStagesList {

  def apply[C <: Coproduct, P <: HList](p: P)(implicit instance: ToStagesList[C, P]): NonEmptyList[C] =
    instance.toList(p)

  implicit def single[H, H1 <: H, TC <: Coproduct]: ToStagesList[H :+: TC, H1 :: HNil] =
    (p: H1 :: HNil) ⇒ NonEmptyList.one(Inl(p.head))

  implicit def every[H, H1 <: H, TC <: Coproduct, TP <: HList](
    implicit t: ToStagesList[TC, TP]
  ): ToStagesList[H :+: TC, H1 :: TP] =
    (p: H1 :: TP) ⇒ NonEmptyList.one(Inl(p.head)) ::: t.toList(p.tail).map(Inr.apply)
}
