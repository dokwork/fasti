package ru.dokwork.fasti.persistence

import cats.data.NonEmptyList

trait SagaPersistence[F[_], Encoded, Id] {

  def persist(id: Id, state: Encoded): F[Unit]

  def lastCompensated(id: Id, cause: Throwable): F[Unit]

  def completed(id: Id): F[Unit]

  def load(id: Id): F[(NonEmptyList[Encoded], Option[Throwable])]
}