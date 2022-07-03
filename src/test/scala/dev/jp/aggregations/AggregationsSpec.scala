package dev.jp.aggregations

import org.scalatest._
import freespec._
import matchers.should.Matchers._
import org.scalactic.TypeCheckedTripleEquals._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import cats._
import cats.implicits._
import fs2.Stream

class AggregationsSpec extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks:
  import Aggregation._

  def pureStreamOf[E, C[_]: Foldable](elements: C[E]): Stream[Id, E] =
    Stream.evals[Id, C, E](elements)

  val buildVector: Aggregation[String, Vector[String]] =
    fold[String, Vector[String]](Vector.empty) { _ :+ _ }

  "fold" - {
    "aggregates over stream" in {
      forAll { (aVector: Vector[String]) =>
        buildVector(pureStreamOf(aVector)) should ===(aVector)
      }
    }

    "can be safely applied multiple times" in {
      import collection.mutable.ArrayBuffer
      val buildBuffer =
        fold[Int, ArrayBuffer[Int]](ArrayBuffer.empty) {
          _.addOne(_)
        }

      //The point here is to prove that elements from the second run don't just get added to the same buffer
      forAll { (aVector: Vector[Int]) =>
        buildBuffer(pureStreamOf(aVector)).toList should ===(
          buildBuffer(pureStreamOf(aVector)).toList
        )
      }

    }
  }

  "foldM" in {
    forAll { (aVector: Vector[String]) =>
      foldM[String](pureStreamOf(aVector)) should ===(aVector.mkString)
    }
  }

  "count" in {
    forAll { (aVector: Vector[String]) =>
      count(pureStreamOf(aVector)) should ===(aVector.length)
    }
  }

  "sum" in {
    forAll { (aVector: Vector[Int]) =>
      sum[Int].apply(pureStreamOf(aVector)) should ===(aVector.sum)
    }
  }

  "mean" - {
    "aggregates no result for empty streams" in {
      mean[Double].apply(pureStreamOf(List.empty[Double])) should ===(None)
    }

    "aggregates to the mean value for non-empty streams" in {
      forAll { (aVector: Vector[Double]) =>
        whenever(aVector.nonEmpty) {
          val expectedMean = aVector.sum / aVector.length
          mean[Double].apply(pureStreamOf(aVector)) should ===(
            Some(expectedMean)
          )
        }
      }
    }
  }

  "foldMap" in {
    val foldMapToString = foldMap[Int, String](_.toString)
    forAll { (aVector: Vector[Int]) =>
      foldMapToString(pureStreamOf(aVector)) should ===(
        aVector.map(_.toString).mkString
      )
    }
  }

  "countOccurencesOf" in {
    forAll { (aVector: Vector[Boolean]) =>
      countOccurencesOf[Boolean].apply(pureStreamOf(aVector)) should === {
        aVector.groupBy(identity).map((a, b) => a -> b.length).toMap
      }
    }
  }

  "when contramapped over" - {

    def appliesTheMapping(
        buildVectorFromInts: Aggregation[Int, Vector[String]]
    ): Unit =
      "applies the mapping before aggregating" in {
        forAll { (aVector: Vector[Int]) =>
          buildVectorFromInts(pureStreamOf(aVector)) should === {
            aVector.map(_.toString)
          }
        }
      }

    "directly" - {
      behave like appliesTheMapping(buildVector.contramap[Int](_.toString()))
    }

    "via the type class" - {
      behave like appliesTheMapping {
        Contravariant[[T] =>> Aggregation[T, Vector[String]]]
          .contramap[String, Int](buildVector)(_.toString)
      }
    }
  }

  "composition" in {
    val composedAggregation = (buildVector, foldM[String]) mapN {
      (elements, concatenated) =>
        elements :+ concatenated
    }

    forAll { (aVector: Vector[String]) =>
      composedAggregation(pureStreamOf(aVector)) should ===(
        aVector :+ aVector.mkString
      )
    }
  }
