package ru.dokwork.fasti

import shapeless.Coproduct

case class UnhandledException[C <: Coproduct](stage: C, cause: Throwable) extends Exception
