package ru.dokwork.fasti

import cats.MonadError
import cats.implicits._
import shapeless._
import shapeless.ops.hlist.Prepend

private[fasti] sealed trait UntypedForward[F[_], B]

private[fasti] sealed trait Forward[F[_], +A, B, S <: HList] extends UntypedForward[F, B]

private[fasti] object Forward {

  implicit final class ForwardOps[F[_], A, B, S <: HList](val self: Forward[F, A, B, S]) {

    def apply[P <: HList](a: A)(implicit F: MonadError[F, Throwable]): F[Either[(HList, Throwable), B]] =
      Forward.execute(HNil)(self, a :: HNil).asInstanceOf[F[Either[(HList, Throwable), B]]]

    def continue[P <: HList](p: P)(implicit F: MonadError[F, Throwable], ev: BeginFrom[S, P]): F[Either[(HList, Throwable), B]] = {
      require(ev ne null)

      def rev(acc: HList): PartialFunction[HList, HList] = {
        case x :: tail ⇒ rev(x :: acc)(tail)
        case HNil ⇒ acc
      }

      Forward.execute(rev(HNil)(p))(self, skipped :: p).asInstanceOf[F[Either[(HList, Throwable), B]]]
    }

    def andThen[C, S2 <: HList](other: Forward[F, B, C, S2])(implicit ev: Prepend[S, B :: S2]): Forward[F, A, C, ev.Out] = {
      require(ev ne null)
      Forward.andThen(self, other).asInstanceOf[Forward[F, A, C, ev.Out]]
    }

    def breakOn(isFatal: PartialFunction[Throwable, Unit]): Forward[F, A, B, S] =
      Forward.breakOn(self)(isFatal).asInstanceOf[Forward[F, A, B, S]]
  }

  private object skipped

  private case class Run[F[_], +A, B](run: Any ⇒ F[B], isFatal: PartialFunction[Throwable, Unit] = PartialFunction.empty)
    extends Forward[F, A, B, HNil]

  private case class Next[F[_], A, B, C, S <: HList](first: Run[F, A, B], next: UntypedForward[F, C])
    extends Forward[F, A, C, B :: S]

  def apply[F[_], A, B](f: A ⇒ F[B]): Forward[F, A, B, HNil] = Run[F, A, B](f.asInstanceOf[Any ⇒ F[B]])

  // in case of exception returned hlist will be reversed
  private def execute[F[_], B](completed: HList)(f0: UntypedForward[F, B], p0: HList)(
    implicit F: MonadError[F, Throwable]
  ): F[Either[(HList, Throwable), _]] = {

    def attempt(f: F[_], isFatal: PartialFunction[Throwable, Unit]): F[Either[(HList, Throwable), _]] =
      f.attempt.flatMap {
        case Left(cause) if isFatal.isDefinedAt(cause) ⇒ F.raiseError(cause)
        case Left(cause) ⇒ F.pure(Left(completed → cause))
        case Right(b) ⇒ F.pure(Right(b))
      }

    (f0, p0) match {
      case (Run(f, classifier), x :: HNil) ⇒ attempt(f(x), classifier)
      case (Next(Run(f, classifier), f2), x :: HNil) ⇒
        attempt(f(x), classifier).flatMap {
          case Right(y) ⇒ execute(y :: completed)(f2, y :: HNil)
          case l ⇒ F.pure(l)
        }
      case (Next(_, f2), _ :: p2) ⇒ execute(completed)(f2, p2)
      case (_, p) ⇒ F.raiseError(new IllegalArgumentException(s"Unexpected part of the list $p"))
    }
  }

  private def andThen[F[_]](f1: UntypedForward[F, _], f2: UntypedForward[F, _]): UntypedForward[F, _] = f1 match {
    case r @ Run(_, _) ⇒ Next(r, f2)
    case Next(r, n) ⇒ Next(r, andThen(n, f2))
  }

  private def breakOn[F[_], B](f0: UntypedForward[F, B])(isFatal: PartialFunction[Throwable, Unit]): UntypedForward[F, B] =
    f0 match {
      case r @ Run(_, _) ⇒ r.copy(isFatal = isFatal)
      case Next(r, n) ⇒ Next(r.copy(isFatal = isFatal), breakOn(n)(isFatal))
    }
}

