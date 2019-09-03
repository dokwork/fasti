package ru.dokwork.fasti

import cats.MonadError
import cats.implicits._
import shapeless._

import scala.language.implicitConversions

sealed trait Backward[F[_]]

object Backward {

  def apply[F[_], B](f: (B, Throwable) ⇒ F[Unit]): Backward[F] =
    Run[F, B](f.asInstanceOf[(Any, Throwable) ⇒ F[Unit]])

  trait BackwardOps[F[_]] {
    def self: Backward[F]

    def apply[P <: HList](p: P, cause: Throwable)(implicit F: MonadError[F, Throwable]): F[Unit] = {
      val b = skip(size(self) - hsize(p))(self)
      Backward.execute(b, p, cause)
    }

    def compose(other: Backward[F]): Backward[F] = Backward.compose(self, other)
  }

  implicit def syntax[F[_]](b: Backward[F]): BackwardOps[F] =
    new BackwardOps[F] {
      override def self: Backward[F] = b
    }

  private case class Run[F[_], B](f: (Any, Throwable) ⇒ F[Unit]) extends Backward[F]

  private case class Next[F[_], S <: HList, C](f: Run[F, C], next: Backward[F]) extends Backward[F]

  // p0 should be reversed
  private def execute[F[_]](b: Backward[F], p0: HList, cause: Throwable)(implicit F: MonadError[F, Throwable]): F[Unit] = {
    (b, p0) match {
      case (_, HNil) ⇒ F.unit
      case (Next(Run(f), next), x :: tail) ⇒ f(x, cause) >> execute(next, tail, cause)
      case (Run(f), head :: HNil) ⇒ f(head, cause)
      case (_, p) ⇒ F.raiseError(new IllegalArgumentException(s"Unexpected part of the list $p"))
    }
  }

  // f': a => b; g': b => c
  // f: b => Unit; g: c => Unit
  // c-b-a
  // Next(g, f)
  private def compose[F[_]](f: Backward[F], g: Backward[F]): Backward[F] = g match {
    case r @ Run(_) ⇒ Next(r, f)
    case Next(r, g0) ⇒ Next(r, compose(f, g0))
  }

  private def size[F[_]]: PartialFunction[Backward[F], Int] = {
    case Run(_) ⇒ 1
    case Next(_, n) ⇒ 1 + size(n)
  }

  private def hsize[P <: HList]: PartialFunction[P, Int] = {
    case HNil ⇒ 0
    case _ :: HNil ⇒ 1
    case _ :: tail ⇒ 1 + hsize(tail)
  }

  private def skip[F[_]](count: Int): PartialFunction[Backward[F], Backward[F]] = {
    case Next(_, n) if count > 0 ⇒ skip(count - 1)(n)
    case b ⇒ b
  }
}
