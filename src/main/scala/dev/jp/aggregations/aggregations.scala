package dev.jp.aggregations

import scala.math.Numeric.Implicits._

trait Aggregation[TIn, TOut]:
  def start(): AggregationRun[TIn, TOut]

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

  final def sum[T: Numeric]: Aggregation[T, T] = fold(Numeric[T].zero) {
    (s, next) =>
      s + next
  }
