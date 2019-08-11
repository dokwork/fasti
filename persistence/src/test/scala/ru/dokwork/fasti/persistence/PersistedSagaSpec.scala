package ru.dokwork.fasti.persistence

import cats.implicits._
import org.scalatest.FreeSpec
import org.scalatest.Matchers.{ a ⇒ _, _ }
import org.scalatest.TryValues._
import ru.dokwork.fasti.Saga
import shapeless.{ ::, HNil }

import scala.util.Try

class PersistedSagaSpec extends FreeSpec {

  trait TestSaga { self: Fixture ⇒
    val saga: CompletedPersistedSaga[Try, A, D, B :: C :: HNil, Int, Encoded] =
      PersistedSaga[Int, Encoded](action[A, B], compensate[B])(_ ⇒ defaultId) andThen
        PersistedSaga[Int, Encoded](action[B, C], compensate[C])(_ ⇒ defaultId) completeOn Saga(action[C, D])
  }

  "PersistedSaga" - {
    "should persist every state" in new Fixture with TestSaga {
      // when:
      saga(defaultId, a)
      // then:
      persistence.storage should contain theSameElementsInOrderAs List("b", "c")
    }
    "on restore" - {
      "should be successfully completed" in new Fixture with TestSaga {
        // given:
        persistence.storage += encoderB.encode(b)

        // when:
        val result = saga.restore(defaultId)

        // then:
        result.success.value shouldBe Right(d)
        completedStages should contain (c)
      }
      "should be successfully compensated" in new Fixture with TestSaga {
        // given:
        persistence.storage ++= List(encoderB.encode(b), encoderC.encode(c))
        persistence.error = Some(testException)

        // when:
        val result = saga.restore(defaultId)

        // then:
        result.success.value shouldBe Left(testException)
        compensatedStages should contain theSameElementsInOrderAs List(c, b)
      }
      "should be failed if a list contains elements in illegal order" in new Fixture with TestSaga {
        // given:
        persistence.storage ++= List(encoderC.encode(c), encoderB.encode(b))

        // when:
        val result = saga.restore(defaultId)

        // then:
        result shouldBe 'failure
        completedStages shouldBe empty
        compensatedStages shouldBe empty
      }
      "should be failed if a list contains extra elements" in new Fixture with TestSaga {
        // given:
        persistence.storage ++= List(encoderB.encode(b), encoderC.encode(c), encoderC.encode(c))

        // when:
        val result = saga.restore(defaultId)

        // then:
        result shouldBe 'failure
        completedStages shouldBe empty
        compensatedStages shouldBe empty
      }
    }
  }
}

