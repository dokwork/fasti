package ru.dokwork.fasti.persistence

import scala.util.Try

trait Decoder[A, Encoded] {
  def decode(x: Encoded): Try[A]
}

object Decoder {
  def apply[A, Encoded](implicit instance: Decoder[A, Encoded]): Decoder[A, Encoded] = instance
}