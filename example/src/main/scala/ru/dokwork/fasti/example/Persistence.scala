package ru.dokwork.fasti.example

import java.util.concurrent.ConcurrentHashMap

import cats.ApplicativeError
import cats.data.NonEmptyList
import io.circe.Json
import ru.dokwork.fasti.persistence.SagaPersistence

class Persistence[F[_]](store: ConcurrentHashMap[OrderId, (List[Json], Option[Throwable])])(
    implicit F: ApplicativeError[F, Throwable]
) extends SagaPersistence[F, Json, OrderId] {

  override def persist(id: OrderId, state: Json): F[Unit] =
    F.pure(store.compute(id, (_, v) => {
      val (list, cause) = Option(v).getOrElse(List.empty → None)
      (list :+ state) → cause
    }))

  override def lastCompensated(id: OrderId, cause: Throwable): F[Unit] =
    F.pure(store.computeIfPresent(id, (_, v) => {
      val (list, _) = v
      list.tail → Some(cause)
    }))

  override def completed(id: OrderId): F[Unit] =
    F.pure(store.remove(id))

  override def load(id: OrderId): F[(NonEmptyList[Json], Option[Throwable])] =
    store.get(id) match {
      case (x :: xs, error) => F.pure(NonEmptyList.of(x, xs: _*) -> error)
      case _ => F.raiseError(new IllegalStateException(s"States for $id was not found"))
    }
}
