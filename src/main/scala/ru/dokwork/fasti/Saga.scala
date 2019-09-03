package ru.dokwork.fasti

import cats.MonadError
import cats.implicits._
import shapeless.ops.hlist.{ Prepend, Reverse }
import shapeless.{ ::, HList, HNil }

final class Saga[F[_], A, B, S <: HList] private(
  private val run: Forward[F, A, B, S],
  private val compensation: Backward[F]
)(implicit F: MonadError[F, Throwable]) extends (A ⇒ F[Either[Throwable, B]]) {

  override def apply(x: A): F[Either[Throwable, B]] = run(x).flatMap(handleFail)

  def continue[P <: HList](p: P)(implicit ev: S BeginFrom P, rev: Reverse[P]): F[Either[Throwable, B]] =
    run.continue(p).flatMap(handleFail)

  private def handleFail(result: Either[(HList, Throwable), B]): F[Either[Throwable, B]] = result match {
    case Right(b) ⇒
      F.pure(Right(b))
    case Left((states, cause)) ⇒
      compensation(states, cause) as Either.left[Throwable, B](cause) handleErrorWith raiseCompensationFailed
  }

  def compensate[P <: HList](p: P, cause: Throwable)(implicit ev: S BeginFrom P, rev: Reverse[P]): F[Left[Throwable, Nothing]] = {
    require(ev ne null)
    (compensation(rev(p), cause) as Left(cause)) handleErrorWith raiseCompensationFailed
  }

  def andThen[S2 <: HList, D](other: Saga[F, B, D, S2])(implicit ev: Prepend[S, B :: S2]): Saga[F, A, D, ev.Out] = {
    require(ev ne null)
    new Saga[F, A, D, ev.Out](
      run andThen other.run,
      compensation compose other.compensation
    )
  }

  private def raiseCompensationFailed[T]: Throwable ⇒ F[T] =
    e ⇒ F.raiseError(CompensationFailed(e))
}

object Saga {

  def apply[F[_], A, B](action: A ⇒ F[B], compensate: (B, Throwable) ⇒ F[Unit])(implicit F: MonadError[F, Throwable]): Saga[F, A, B, HNil] =
    new Saga[F, A, B, HNil](Forward(action), Backward(compensate))

  def apply[F[_], A, B](action: A ⇒ F[B])(implicit F: MonadError[F, Throwable]): Saga[F, A, B, HNil] =
    apply(action, (_, _) ⇒ F.unit)
}