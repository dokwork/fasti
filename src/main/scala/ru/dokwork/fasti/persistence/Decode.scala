package ru.dokwork.fasti.persistence

import scala.util.Try

trait Decode[A, E] {
  def decode(x: E): Try[A]
}

object Decode {
  def apply[A, E](implicit instance: Decode[A, E]): Decode[A, E] = instance
}