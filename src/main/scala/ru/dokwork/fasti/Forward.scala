package ru.dokwork.fasti

import cats.{ Monad, MonadError }
import cats.implicits._
import ru.dokwork.fasti.shapeless_ops.Join
import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }

trait Forward[F[_], A, B, C <: Coproduct] extends (Either[A, C] => F[(List[C], Either[Throwable, B])]) {
  first =>

  type Result = (List[C], Either[Throwable, B])

  def andThen[D, C1 <: Coproduct](other: Forward[F, B, D, C1])(
      implicit
      F: Monad[F],
      join: Join[C, B :+: C1]
  ): Forward[F, A, D, join.Out] = new Forward[F, A, D, join.Out] {

    override def apply(x: Either[A, join.Out]): F[Result] = {
      def continue(res1: first.Result): F[Result]= res1 match {
        case (list, Left(e)) => F.pure(list.map(l => join.left(l)) -> Left(e))
        case (list, Right(b)) =>
          F.map(apply(Right(join.right(Inl(b))))) {
            case (list2, res) => (list.map(l=> join.left(l)) ++ list2) -> res
          }
      }

      def toResult(res2: other.Result): Result = res2 match {
        case (list, res) => list.map(c1 => join.right(Inr(c1))) -> res
      }
      def addB(b: B)(res: Result): Result = res match {
        case (list, r) => (join.right(Inl(b)) +: list) -> r
      }

      x match {
        case Left(a) => first(Left(a)).flatMap(res1 => continue(res1))
        case Right(out) =>
          join.separate(out) match {
            case Left(c) => first(Right(c)).flatMap(res1 => continue(res1))
            case Right(Inl(b)) => other(Left(b)).map(res2 => addB(b)(toResult(res2)))
            case Right(Inr(c1)) => other(Right(c1)).map(res2 => toResult(res2))
          }
      }
    }
  }
}

object Forward {

  def apply[F[_], A, B](f: A => F[B])(implicit F: MonadError[F, Throwable]): Forward[F, A, B, CNil] =
    new Forward[F, A, B, CNil] {
      override def apply(x: Either[A, CNil]): F[Result] = x match {
        case Left(a)     => F.map(f(a).attempt)(List.empty -> _)
        case Right(cnil) => cnil.impossible
      }
    }
}
