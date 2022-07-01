package dev.jp.aggregations

import cats._
import cats.implicits._
import fs2.Compiler
import fs2.Stream

import Numeric.Implicits._
import Fractional.Implicits._

//TODO: Add some scaladoc for the more important classes
trait Aggregation[TIn, TOut]:
  def start(): Run[TIn, TOut]

  def apply[F[_]: Functor](
      stream: Stream[F, TIn]
  )(using Compiler[F, F]): F[TOut] =
    stream.compile
      .fold(start()) { _.consume(_) }
      .map(_.result())

  // We define this here instead of relying on cats' extension methods,
  // because Scala has some trouble deriving those
  // (probably due to the additional type parameter)
  def contramap[B](
      f: B => TIn
  ): Aggregation[B, TOut] = () => Aggregation.ContramapRun(f, start())

trait Run[TIn, TOut]:
  def consume(element: TIn): Run[TIn, TOut]
  def result(): TOut

object Aggregation:

  final def fold[TIn, TOut](initial: => TOut)(
      foldStep: (TOut, TIn) => TOut
  ): Aggregation[TIn, TOut] =
    () => FoldRun(initial, foldStep)

  private class FoldRun[TIn, TOut](
      current: TOut,
      foldStep: (TOut, TIn) => TOut
  ) extends Run[TIn, TOut]:
    def consume(element: TIn): Run[TIn, TOut] =
      FoldRun(foldStep(current, element), foldStep)
    def result(): TOut = current

  final def count[T]: Aggregation[T, Int] = fold(0) { (current, _) =>
    current + 1
  }

  final def sum[T: Numeric]: Aggregation[T, T] =
    fold(Numeric[T].zero) { (s, next) =>
      s + next
    }

  final def foldMap[TIn, M: Monoid](
      map: TIn => M
  ): Aggregation[TIn, M] =
    fold(Monoid.empty[M]) { (current, next) =>
      current |+| map(next)
    }

  final def mean[T: Numeric: Fractional]: Aggregation[T, Option[T]] =
    (sum[T], count) mapN { (theSum, theCount) =>
      theCount match {
        case 0 => None
        case _ => Some(theSum / Numeric[T].fromInt(theCount))
      }
    }

  final def countOccurencesOf[T]: Aggregation[T, Map[T, Int]] =
    foldMap(occurence => Map(occurence -> 1))

  given [TIn]: Applicative[[TOut] =>> Aggregation[TIn, TOut]] with

    def pure[A](x: A): Aggregation[TIn, A] = () => PureRun(x)

    def ap[A, B](left: Aggregation[TIn, A => B])(
        right: Aggregation[TIn, A]
    ): Aggregation[TIn, B] = () =>
      ApRun(
        left.start(),
        right.start()
      )

  private class PureRun[TIn, TOut](fixedResult: TOut) extends Run[TIn, TOut]:
    def consume(element: TIn): Run[TIn, TOut] = this
    def result(): TOut = fixedResult

  private class ApRun[TIn, TIntermediate, TOut](
      left: Run[TIn, TIntermediate => TOut],
      right: Run[TIn, TIntermediate]
  ) extends Run[TIn, TOut]:
    def consume(element: TIn): Run[TIn, TOut] =
      ApRun(
        left.consume(element),
        right.consume(element)
      )

    def result(): TOut =
      left.result()(right.result())

  given [TOut]: Contravariant[[TIn] =>> Aggregation[TIn, TOut]] with
    def contramap[A, B](mapped: Aggregation[A, TOut])(
        f: B => A
    ): Aggregation[B, TOut] = mapped.contramap(f)

  private class ContramapRun[TIn, B, TOut](
      map: TIn => B,
      innerRun: Run[B, TOut]
  ) extends Run[TIn, TOut]:

    def consume(element: TIn): Run[TIn, TOut] =
      ContramapRun(map, innerRun.consume(map(element)))

    def result(): TOut = innerRun.result()
