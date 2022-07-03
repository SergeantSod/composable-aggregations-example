package dev.jp.aggregations

import cats._
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import fs2.Pipe
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text
import io.circe.Decoder
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode

import scala.util.Try

import Aggregation._

case class Employee(
    firstName: String,
    lastName: String,
    salary: Double,
    department: Department
)

object Employee:
  given Decoder[Employee] = deriveDecoder[Employee]

enum Department {
  case Engineering, HumanResources, Legal, Sales
}

object Department:
  given Decoder[Department] = Decoder[String].emapTry { raw =>
    Try { Department.valueOf(raw) }
  }

case class Statistics(
    numberOfEmployees: Int,
    totalSalary: Double,
    averageSalary: Option[Double],
    departmentSizes: Map[Department, Int]
)

val staffStatistics: Aggregation[Employee, Statistics] = (
  count[Employee],
  sum[Double].contramap[Employee](_.salary),
  mean[Double].contramap[Employee](_.salary),
  countOccurrencesOf[Department].contramap[Employee](_.department)
) mapN Statistics.apply

def parseJsonLinesAs[T: Decoder]: Pipe[IO, Byte, T] =
  _.through(text.utf8.decode)
    .through(text.lines)
    .evalMap { line =>
      decode[T](line).liftTo[IO]
    }

object Example extends IOApp:

  def run(args: List[String]) =
    for {
      statistics <- staffStatistics(
        Files[IO]
          .readAll(Path("staff.jsonl"))
          .through(parseJsonLinesAs[Employee])
      )
      _ <- IO.println(statistics)
    } yield ExitCode.Success
