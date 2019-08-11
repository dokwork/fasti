package ru.dokwork.fasti

import shapeless._

import scala.annotation.implicitNotFound

@implicitNotFound("${S} don't begin from the ${P}")
trait BeginFrom[S <: HList, P <: HList]

object BeginFrom {
  def apply[S <: HList, P <: HList](implicit instance: BeginFrom[S, P]): BeginFrom[S, P] = instance

  implicit def single[H, T <: HList]: BeginFrom[H :: T, H :: HNil] = new BeginFrom[H :: T, H :: HNil] {}

  implicit def list[H, TS <: HList, TP <: HList](implicit t: BeginFrom[TS, TP]): BeginFrom[H :: TS, H :: TP] =
    new BeginFrom[H :: TS, H :: TP] { require(t ne null) }
}

