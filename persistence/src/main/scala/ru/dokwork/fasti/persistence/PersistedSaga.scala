package ru.dokwork.fasti.persistence

import cats.MonadError
import cats.implicits._
import ru.dokwork.fasti.Saga
import shapeless.ops.hlist.Prepend
import shapeless.{ ::, HList, HNil }

final class PersistedSaga[F[_], A, B, S <: HList, Id, Encoded] private (
    private val inner: Saga[F, A, B, S]
)(implicit F: MonadError[F, Throwable]) {

  def andThen[S2 <: HList, D](other: PersistedSaga[F, B, D, S2, Id, Encoded])(
      implicit ev: Prepend[S, B :: S2]
  ): PersistedSaga[F, A, D, ev.Out, Id, Encoded] =
    new PersistedSaga[F, A, D, ev.Out, Id, Encoded](inner andThen other.inner)

  def completeOn[S2 <: HList, D](other: Saga[F, B, D, S2])(
      implicit ev: Prepend[S, B :: S2]
  ): CompletedPersistedSaga[F, A, D, ev.Out, Id, Encoded] =
    new CompletedPersistedSaga[F, A, D, ev.Out, Id, Encoded](inner andThen other)
}

final class CompletedPersistedSaga[F[_], A, B, S <: HList, Id, Encoded] private[persistence] (
    private val inner: Saga[F, A, B, S]
)(implicit F: MonadError[F, Throwable]) {

  def apply(id: Id, a: A)(implicit persistence: SagaPersistence[F, Encoded, Id]): F[Either[Throwable, B]] =
    inner(a).flatTap(_ ⇒ persistence.completed(id))

  def restore(id: Id)(
      implicit decoder: DecodeStates[S, Encoded],
      persistence: SagaPersistence[F, Encoded, Id]
  ): F[Either[Throwable, B]] =
    persistence
      .load(id)
      .flatMap {
        case (states, error) ⇒
          F.fromTry(decoder.decode(states)).flatMap { x ⇒
            implicit val (p, beginFrom) = x
            error.fold(inner.continue(p).flatTap(_ ⇒ persistence.completed(id)))(
              cause ⇒ inner.compensate(p, cause).asInstanceOf[F[Either[Throwable, B]]]
            )
          }
      }
}

object PersistedSaga {

  def apply[Id, Encoded]: Applier[Id, Encoded] = new Applier[Id, Encoded]

  final class Applier[Id, Encoded] {
    def apply[F[_], A, B](action: A ⇒ F[B], compensate: (B, Throwable) ⇒ F[Unit])(getId: B ⇒ Id)(
        implicit F: MonadError[F, Throwable],
        encoder: Encoder[B, Encoded],
        persistence: SagaPersistence[F, Encoded, Id],
    ): PersistedSaga[F, A, B, HNil, Id, Encoded] = new PersistedSaga(
      Saga(
        a ⇒ action(a).flatTap(b ⇒ persistence.persist(getId(b), encoder.encode(b))),
        (b, cause) ⇒ compensate(b, cause) >> persistence.lastCompensated(getId(b), cause)
      )
    )
  }
}
