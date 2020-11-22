package ru.dokwork.fasti.persistence

trait Encode[A, E] {
  def encode(x: A): E
}

object Encode {
  def apply[A, E](implicit instance: Encode[A, E]): Encode[A, E] = instance
}