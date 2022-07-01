package dev.jp.aggregations

import cats.implicits._
import cats._
import fs2.{Stream, Compiler}

import Numeric.Implicits._
import Fractional.Implicits._

trait Aggregation[TIn, TOut]:
  def start(): AggregationRun[TIn, TOut]

  def contramap[B](
      f: B => TIn
  ): Aggregation[B, TOut] = () => Aggregation.ContraMapRun(f, start())

  def apply[F[_]: Functor](
      stream: Stream[F, TIn]
  )(using Compiler[F, F]): F[TOut] =
    stream.compile
      .fold(start()) { _.consume(_) }
      .map(_.result())

trait AggregationRun[TIn, TOut]:
  def consume(element: TIn): AggregationRun[TIn, TOut]
  def result(): TOut

object Aggregation:

  private class FoldRun[TIn, TOut](
      current: TOut,
      foldStep: (TOut, TIn) => TOut
  ) extends AggregationRun[TIn, TOut]:
    def consume(element: TIn): AggregationRun[TIn, TOut] =
      FoldRun(foldStep(current, element), foldStep)
    def result(): TOut = current

  final def fold[TIn, TOut](initial: => TOut)(
      foldStep: (TOut, TIn) => TOut
  ): Aggregation[TIn, TOut] =
    () => FoldRun(initial, foldStep)

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

  final def countOccurencesOf[T]: Aggregation[T, Map[T, Int]] =
    foldMap(occurence => Map(occurence -> 1))

  final def mean[T: Numeric: Fractional]: Aggregation[T, Option[T]] =
    //TODO: can we write this more simply using map on the count?
    //TODO: Division by zero
    (sum[T], count) mapN { (theSum, theCount) =>
      if theCount == 0 then None
      else Some(theSum / Numeric[T].fromInt(theCount))
    }

  private class PureRun[TIn, TOut](fixedResult: TOut)
      extends AggregationRun[TIn, TOut]:
    def consume(element: TIn): AggregationRun[TIn, TOut] = this
    def result(): TOut = fixedResult

  private class ApRun[TIn, TIntermediate, TOut](
      left: AggregationRun[TIn, TIntermediate => TOut],
      right: AggregationRun[TIn, TIntermediate]
  ) extends AggregationRun[TIn, TOut]:
    def consume(element: TIn): AggregationRun[TIn, TOut] =
      ApRun(
        left.consume(element),
        right.consume(element)
      )

    def result(): TOut =
      left.result()(right.result())

  //TODO: Check if we can write the type lambdas more succinctly
  given [TIn]: Applicative[[TOut] =>> Aggregation[TIn, TOut]] with

    def pure[A](x: A): Aggregation[TIn, A] = () => PureRun(x)

    def ap[A, B](left: Aggregation[TIn, A => B])(
        right: Aggregation[TIn, A]
    ): Aggregation[TIn, B] = () =>
      ApRun(
        left.start(),
        right.start()
      )

  //TODO: Move the classes around a little
  private class ContraMapRun[TIn, B, TOut](
      map: TIn => B,
      innerRun: AggregationRun[B, TOut]
  ) extends AggregationRun[TIn, TOut]:
    def consume(element: TIn): AggregationRun[TIn, TOut] =
      ContraMapRun(map, innerRun.consume(map(element)))
    def result(): TOut = innerRun.result()

  given [TOut]: Contravariant[[TIn] =>> Aggregation[TIn, TOut]] with
    def contramap[A, B](mapped: Aggregation[A, TOut])(
        f: B => A
    ): Aggregation[B, TOut] = mapped.contramap(f)
