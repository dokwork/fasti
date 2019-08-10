package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import org.scalatest.TryValues._
import org.scalatest.{ FreeSpec, Matchers }
import shapeless.test.illTyped
import shapeless.{ :+:, CNil, _ }

import scala.util.Try

class DecodeStagesSpec extends FreeSpec with Matchers {
  implicit val intDecode: Decode[Int, String] = (x: String) => Try(x.toInt)
  implicit val doubleDecode: Decode[Double, String] = (x: String) => Try(x.toDouble)
  implicit val boolDecode: Decode[Boolean, String] = (x: String) => Try(x.toBoolean)

  "derivation" - {
    "should not derive decoder for CNil" in {
      illTyped("implicitly[DecodeStages[CNil, String]]")
    }
    "should not derive decoder if coproduct contains type without decoder" in {
      illTyped("implicitly[DecodeStages[Symbol :+: CNil, String]]")
    }
    "should derive decoder for single element coproduct" in {
      implicitly[DecodeStages[Int :+: CNil, String]]
    }
    "should derive decoder for arbitrary coproduct" in {
      implicitly[DecodeStages[Int :+: Double :+: Boolean :+: CNil, String]]
    }
  }

  "decode" - {
    type C = Int :+: Double :+: Boolean :+: CNil
    val decoder = implicitly[DecodeStages[C, String]]

    def c = new Coproduct.MkCoproduct[C]

    "should decode full list" in {
      decoder.decode(NonEmptyList.of("42", "100.500", "true")).success.value shouldBe NonEmptyList.of(c(42), c(100.5), c(true))
    }
    "should decode partial list" in {
      decoder.decode(NonEmptyList.of("42")).success.value shouldBe NonEmptyList.of(c(42))
      decoder.decode(NonEmptyList.of("42", "100.500")).success.value shouldBe NonEmptyList.of(c(42), c(100.5))
    }
    "should fail with IllegalArgumentException on decode list with extra element" in {
      decoder.decode(NonEmptyList.of("42", "100.500", "true", "'extra")).failure.exception shouldBe an[IllegalArgumentException]
    }
    "should fail with NumberFormatException on decode list with incorrect element" in {
      decoder.decode(NonEmptyList.of("42!")).failure.exception shouldBe an[NumberFormatException]
    }
  }
}
