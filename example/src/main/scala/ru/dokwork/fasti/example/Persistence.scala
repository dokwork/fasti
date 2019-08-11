package ru.dokwork.fasti.example


import java.util.concurrent.ConcurrentHashMap

import cats.Applicative
import io.circe.Json
import ru.dokwork.fasti.persistence.SagaPersistence

class Persistence[F[_] : Applicative](store: ConcurrentHashMap[OrderId, (List[Json], Option[Throwable])])
  extends SagaPersistence[F, Json, OrderId] {

  override def persist(id: OrderId, state: Json): F[Unit] =
    Applicative[F].pure(store.compute(id, (_, v) ⇒ {
      val (list, cause) = Option(v).getOrElse(List.empty → None)
      (list :+ state) → cause
    }))

  override def lastCompensated(id: OrderId, cause: Throwable): F[Unit] =
    Applicative[F].pure(store.computeIfPresent(id, (_, v) ⇒ {
      val (list, _) = v
      list.tail → Some(cause)
    }))

  override def completed(id: OrderId): F[Unit] =
    Applicative[F].pure(store.remove(id))

  override def load(id: OrderId): F[(List[Json], Option[Throwable])] =
    Applicative[F].pure {
      val v = List.empty[Json] → None
      Option(store.putIfAbsent(id, v)).getOrElse(v)
    }
}