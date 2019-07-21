package ru.dokwork.fasti

import cats.MonadError
import cats.implicits._
import ru.dokwork.fasti.shapeless_ops.Join
import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }

trait Backward[F[_], B, C <: Coproduct] extends ((Either[B, C], Throwable) => F[Either[Throwable, Unit]]) { self =>
  def or[C1 <: Coproduct, D](other: Backward[F, D, C1])(implicit join: Join[C, B :+: C1]): Backward[F, D, join.Out] =
    Backward[F, D, join.Out] { (x: Either[D, join.Out], err: Throwable) =>
      x match {
        case Left(d) => other(Left(d), err)
        case Right(out) =>
          join.separate(out) match {
            case Left(c)        => self(Right(c), err)
            case Right(Inl(b))  => self(Left(b), err)
            case Right(Inr(c1)) => other(Right(c1), err)
          }
      }
    }
}

object Backward {
  def apply[F[_], B, C <: Coproduct](run: (Either[B, C], Throwable) => F[Either[Throwable, Unit]]): Backward[F, B, C] =
    (v1: Either[B, C], v2: Throwable) => run(v1, v2)

  def apply[F[_]: MonadError[?[_], Throwable], B](f: (B, Throwable) => F[Unit]): Backward[F, B, CNil] = {
    (arg: Either[B, CNil], err: Throwable) =>
      arg match {
        case Left(b)  => f(b, err).attempt
        case Right(c) => c.impossible
      }
  }
}
