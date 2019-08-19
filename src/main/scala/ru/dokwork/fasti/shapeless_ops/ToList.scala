package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import shapeless.{ ::, _ }

import scala.annotation.implicitNotFound

@implicitNotFound("[${P}] can't be represented as list of [${C}]. ")
trait ToList[P, C <: Coproduct] {
  def toList(p: P): NonEmptyList[C]
}

object ToList {

  def apply[P <: HList, C <: Coproduct](p: P)(implicit instance: ToList[P, C]): NonEmptyList[C] =
    instance.toList(p)

  implicit def single[H, H1 <: H, TC <: Coproduct]: ToList[H1 :: HNil, H :+: TC] =
    (p: H1 :: HNil) ⇒ NonEmptyList.one(Inl(p.head))

  implicit def every[H, H1 <: H, TC <: Coproduct, TP <: HList](
    implicit t: ToList[TP, TC]
  ): ToList[H1 :: TP, H :+: TC] =
    (p: H1 :: TP) ⇒ NonEmptyList.one(Inl(p.head)) ::: t.toList(p.tail).map(Inr.apply)
}