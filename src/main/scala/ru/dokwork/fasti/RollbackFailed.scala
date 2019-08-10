package ru.dokwork.fasti

import shapeless.Coproduct

case class RollbackFailed[C <: Coproduct](stage: C, cause: Throwable) extends Exception
