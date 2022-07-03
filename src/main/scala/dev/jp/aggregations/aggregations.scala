package dev.jp.aggregations

import cats._
import cats.implicits._
import fs2.Compiler
import fs2.Stream

import Numeric.Implicits._
import Fractional.Implicits._

/** A resuable, composable object that can aggregate a stream of values of type
  * `TIn` into a result of type `TOut`
  */
trait Aggregation[TIn, TOut]:

  /** Starts a run across a stream */
  def start(): Run[TIn, TOut]

  /** Runs this aggregation over a Stream */
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

/** A running aggregation that may consume more elements of type `TIn` from a
  * stream or produce a result of type `TOut` at the end of the stream.
  */
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

  final def foldM[M](using m: Monoid[M]): Aggregation[M, M] =
    fold(m.empty)(m.combine)

  final def count[T]: Aggregation[T, Int] =
    fold(0) { (current, _) => current + 1 }

  final def sum[T: Numeric]: Aggregation[T, T] =
    fold(Numeric[T].zero) { (s, next) => s + next }

  final def foldMap[TIn, M: Monoid](map: TIn => M): Aggregation[TIn, M] =
    foldM[M].contramap(map)

  final def mean[T: Fractional]: Aggregation[T, Option[T]] =
    (sum[T], count[T]) mapN { (theSum, theCount) =>
      theCount match {
        case 0 => None
        case _ => Some(theSum / Fractional[T].fromInt(theCount))
      }
    }

  final def countOccurrencesOf[T]: Aggregation[T, Map[T, Int]] =
    foldMap(occurrence => Map(occurrence -> 1))

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
