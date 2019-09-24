package ru.dokwork.fasti.persistence

trait Encoder[A, Encoded] {
  def encode(x: A): Encoded
}

object Encoder {
  def apply[A, Encoded](implicit instance: Encoder[A, Encoded]): Encoder[A, Encoded] = instance
}