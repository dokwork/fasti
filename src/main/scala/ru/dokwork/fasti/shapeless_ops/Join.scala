package ru.dokwork.fasti.shapeless_ops

import cats.implicits._
import shapeless.ops.coproduct.Prepend
import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }

import scala.annotation.implicitNotFound

@implicitNotFound("Coproduct ${L} can't be joined with ${R}")
trait Join[L <: Coproduct, R <: Coproduct] extends Prepend[L, R] {
  type Out <: Coproduct

  def left(x: L): Out = apply(Left(x))
  def right(x: R): Out = apply(Right(x))
  def separate(c: Out): Either[L, R]
}

object Join {
  type Aux[P <: Coproduct, S <: Coproduct, Out0 <: Coproduct] = Join[P, S] { type Out = Out0 }

  def apply[P <: Coproduct, S <: Coproduct](implicit join: Join[P, S]): Aux[P, S, join.Out] = join

  implicit def cnilJoin[S <: Coproduct]: Aux[CNil, S, S] =
    new Join[CNil, S] {
      type Out = S
      def apply(e: Either[CNil, S]): S    = e.right.get
      def separate(c: S): Either[CNil, S] = Right(c)
    }

  implicit def cconsJoin[PH, PT <: Coproduct, S <: Coproduct](
      implicit pt: Join[PT, S]
  ): Aux[PH :+: PT, S, PH :+: pt.Out] =
    new Join[PH :+: PT, S] {
      type Out = PH :+: pt.Out
      def apply(e: Either[PH :+: PT, S]): Out = e match {
        case Left(Inl(h)) => Inl(h)
        case Left(Inr(t)) => Inr(pt(Left(t)))
        case Right(s)     => Inr(pt(Right(s)))
      }
      def separate(c: PH :+: pt.Out): Either[PH :+: PT, S] = c match {
        case Inl(x) => Left(Inl(x))
        case Inr(ptout)  => pt.separate(ptout).leftMap(p => Inr(p))
      }
    }
}
