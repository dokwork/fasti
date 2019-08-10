package ru.dokwork.fasti

import cats.MonadError
import cats.data.NonEmptyList
import cats.implicits._
import ru.dokwork.fasti.Saga.Result
import ru.dokwork.fasti.shapeless_ops._
import shapeless.{ :+:, CNil, Coproduct, HList }

import scala.util.{ Failure, Success }

final class Saga[F[_], A, B, S <: Coproduct] private(
  private val run: Forward[F, A, B, S],
  private val compensate: Backward[F, B, S]
)(implicit F: MonadError[F, Throwable]) {
  self =>

  def apply(x: A): F[Result[B]] = continue0(List.empty, run(Left(x)))

  def continue[Encoded](completedStages: NonEmptyList[Encoded])(
    implicit
    decoder: DecodeStages[S, Encoded]
  ): F[Result[B]] = {
    decoder.decode(completedStages) match {
      case Success(stages) ⇒ continue0(stages.init, run(Right(stages.last)))
      case Failure(err) ⇒ F.raiseError(err)
    }
  }

  def continue[P <: HList](completedStages: P)(
    implicit
    toSL: ToStagesList[S, P]
  ): F[Result[B]] = {
    val done = toSL.toList(completedStages)
    continue0(done.init, run(Right(done.last)))
  }

  def rollback[Encoded](completedStages: NonEmptyList[Encoded], reason: Throwable)(
    implicit
    decoder: DecodeStages[S, Encoded]
  ): F[Result.Rolledback[B]] = {
    decoder.decode(completedStages) match {
      case Success(stages) ⇒ rollback0(stages.toList, reason)
      case Failure(err) ⇒ F.raiseError(err)
    }
  }

  def rollback[P <: HList](completedStages: P, reason: Throwable)(
    implicit
    toSL: ToStagesList[S, P]
  ): F[Result.Rolledback[B]] = rollback0(toSL.toList(completedStages).toList, reason)

  private def continue0(done: List[S], result: F[run.Result]): F[Result[B]] =
    F.flatMap(result) {
      case (_, Right(b)) =>
        F.pure(Result.Success(b))

      case (handled, Left(err)) =>
        F.map(rollback0(done ++ handled, err))(identity[Result[B]])
    }

  private def rollback0(stages: List[S], cause: Throwable): F[Result.Rolledback[B]] = {
    import scala.collection.immutable.::

    def loop(stages: List[S]): F[Result.Rolledback[B]] = stages match {
      case head :: tail =>
        compensate(Right(head), cause).flatMap {
          case Right(_) if tail.nonEmpty => loop(tail)
          case Right(_) => F.pure(Result.Rolledback(cause))
          case Left(e) => F.raiseError(UnhandledException(head, e))
        }
      case Nil => F.pure(Result.Rolledback(cause))
    }

    loop(stages)
  }

  def andThen[S2 <: Coproduct, D](other: Saga[F, B, D, S2])(
    implicit
    join: Join[S, B :+: S2]
  ): Saga[F, A, D, join.Out] =
    new Saga[F, A, D, join.Out](
      run = run andThen other.run,
      compensate = compensate or other.compensate
    )
}

object Saga {

  sealed trait Result[T]

  object Result {
    case class Success[T](result: T) extends Result[T]
    case class Rolledback[T](cause: Throwable) extends Result[T]
  }

  def apply[F[_], A, B](
    action: A => F[B],
    rollback: (B, Throwable) => F[Unit]
  )(implicit F: MonadError[F, Throwable]): Saga[F, A, B, CNil] = new Saga(
    run = Forward(action),
    compensate = Backward(rollback)
  )

  def apply[F[_], A, B](
    action: A => F[B]
  )(implicit F: MonadError[F, Throwable]): Saga[F, A, B, CNil] =
    apply(action, (_: B, _: Throwable) => F.unit)
}
