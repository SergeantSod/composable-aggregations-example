package dev.jp.aggregations

import org.scalatest._
import freespec._
import matchers.should.Matchers._
import org.scalactic.TypeCheckedTripleEquals._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import cats.Foldable
import cats.Id
import fs2.Stream

class AggregationsSpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks:
  import Aggregation._

  def pureStreamOf[E, C[_]: Foldable](elements: C[E]): Stream[Id, E] =
    Stream.evals[Id, C, E](elements)

  "fold" - {
    "aggregates over stream" in {
      val buildVector = fold[Int, Vector[Int]](Vector.empty) { _ :+ _ }

      forAll { (aVector: Vector[Int]) =>
        buildVector(pureStreamOf(aVector)) should ===(aVector)
      }
    }

    "can be safely applied multiple times" in {
      import collection.mutable.ArrayBuffer
      val buildBuffer =
        fold[Int, ArrayBuffer[Int]](ArrayBuffer.empty) {
          _.addOne(_)
        }

      forAll { (aVector: Vector[Int]) =>
        buildBuffer(pureStreamOf(aVector)).toList should ===(
          buildBuffer(pureStreamOf(aVector)).toList
        )
      }

    }
  }
