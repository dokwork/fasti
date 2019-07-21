package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import org.scalatest.{ FreeSpec, Matchers }
import shapeless._
import shapeless.test.illTyped

class ToStagesListSpec extends FreeSpec with Matchers {

  type A = Boolean
  type B = Int
  type C = String

  "deriving" - {
    "should not derive instances for the empty product and coproduct" in {
      illTyped("implicitly[ToStagesList[CNil, HNil]]")
    }

    "should derive instance for single product and coproduct" in {
      implicitly[ToStagesList[A :+: CNil, A :: HNil]]
    }

    "should derive instance for equal product and coproduct" in {
      implicitly[ToStagesList[A :+: B :+: C :+: CNil, A :: B :: C :: HNil]]
    }

    "should not derive instance for product with illegal order of the types" in {
      illTyped("implicitly[ToStagesList[A :+: B :+: C :+: CNil, B :: C :: A :: HNil]]")
    }

    "should derive instance for the partial product" in {
      implicitly[ToStagesList[A :+: B :+: C :+: CNil, A :: HNil]]
      implicitly[ToStagesList[A :+: B :+: C :+: CNil, A :: B :: HNil]]
    }
  }
  "converting" - {
    "should correctly convert single-element product" in {
      ToStagesList[A :+: CNil, A :: HNil](true :: HNil) shouldBe NonEmptyList.of(Inl(true))
    }
    "should correctly convert full product" in {
      ToStagesList[A :+: B :+: C :+: CNil, A :: B :: C :: HNil](true :: 0 :: "" :: HNil) shouldBe NonEmptyList.of(
        Inl(true),
        Inr(Inl(0)),
        Inr(Inr(Inl("")))
      )
    }
    "should correctly convert partial product" in {
      ToStagesList[A :+: B :+: C :+: CNil, A :: B :: HNil](true :: 0 :: HNil) shouldBe NonEmptyList.of(
        Inl(true),
        Inr(Inl(0))
      )
    }
  }
}
