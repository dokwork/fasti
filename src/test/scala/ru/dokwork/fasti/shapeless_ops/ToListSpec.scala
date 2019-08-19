package ru.dokwork.fasti.shapeless_ops

import cats.data.NonEmptyList
import org.scalatest.{ FreeSpec, Matchers }
import shapeless._
import shapeless.test.illTyped

class ToListSpec extends FreeSpec with Matchers {

  type A = Boolean
  type B = Int
  type C = String

  "For HList" - {
    "deriving" - {
      "should not derive instances for the empty product and coproduct" in {
        illTyped("implicitly[ToStagesList[HNil, CNil]]")
      }

      "should derive instance for single product and coproduct" in {
        implicitly[ToList[A :: HNil, A :+: CNil]]
      }

      "should derive instance for equal product and coproduct" in {
        implicitly[ToList[A :: B :: C :: HNil, A :+: B :+: C :+: CNil]]
      }

      "should not derive instance for product with illegal order of the types" in {
        illTyped("implicitly[ToStagesList[B :: C :: A :: HNil, A :+: B :+: C :+: CNil]]")
      }

      "should derive instance for the partial product" in {
        implicitly[ToList[A :: HNil, A :+: B :+: C :+: CNil]]
        implicitly[ToList[A :: B :: HNil, A :+: B :+: C :+: CNil]]
      }
    }
    "converting" - {
      "should correctly convert single-element product" in {
        ToList[A :: HNil, A :+: CNil](true :: HNil) shouldBe NonEmptyList.of(Inl(true))
      }
      "should correctly convert full product" in {
        ToList[A :: B :: C :: HNil, A :+: B :+: C :+: CNil](true :: 0 :: "" :: HNil) shouldBe NonEmptyList.of(
          Inl(true),
          Inr(Inl(0)),
          Inr(Inr(Inl("")))
        )
      }
      "should correctly convert partial product" in {
        ToList[A :: B :: HNil, A :+: B :+: C :+: CNil](true :: 0 :: HNil) shouldBe NonEmptyList.of(
          Inl(true),
          Inr(Inl(0))
        )
      }
    }
  }
}
