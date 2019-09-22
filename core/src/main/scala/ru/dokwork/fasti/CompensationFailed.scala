package ru.dokwork.fasti

case class CompensationFailed(cause: Throwable) extends Exception(cause)
