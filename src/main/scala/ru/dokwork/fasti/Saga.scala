package ru.dokwork.fasti

import cats.MonadError
import cats.implicits._
import ru.dokwork.fasti.Saga.Result
import ru.dokwork.fasti.shapeless_ops._
import shapeless.{ :+:, CNil, Coproduct }

final class Saga[F[_], A, B, S <: Coproduct] private(
  private val run: Forward[F, A, B, S],
  private val compensate: Backward[F, B, S]
)(implicit F: MonadError[F, Throwable]) {
  self =>

  def apply(x: A): F[Result[B]] = continue0(List.empty, run(Left(x)))

  def continue[P](completedStages: P)(
    implicit
    ev: ToList[P, S]
  ): F[Result[B]] = {
    val done = ev.toList(completedStages)
    continue0(done.init, run(Right(done.last)))
  }

  private def continue0(done: List[S], result: F[run.Result]): F[Result[B]] =
    F.flatMap(result) {
      case (_, Right(b)) =>
        F.pure(Result.Success(b))

      case (handled, Left(err)) =>
        F.map(rollback0(done ++ handled, err))(identity[Result[B]])
    }

  def rollback[P](completedStages: P, reason: Throwable)(
    implicit
    ev: ToList[P, S]
  ): F[Result.Rolledback[B]] = rollback0(ev.toList(completedStages).toList, reason)

  private def rollback0(stages: List[S], cause: Throwable): F[Result.Rolledback[B]] = {
    import scala.collection.immutable.::

    def loop(stages: List[S]): F[Result.Rolledback[B]] = stages match {
      case head :: tail =>
        compensate(Right(head), cause).flatMap {
          case Right(_) if tail.nonEmpty => loop(tail)
          case Right(_) => F.pure(Result.Rolledback(cause))
          case Left(e) => F.raiseError(RollbackFailed(head, e))
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
