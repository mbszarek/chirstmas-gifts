package com.example.quickstart

import cats.Applicative
import scala.annotation.tailrec
import scala.util.Random

trait DrawService[F[_]] {
  def draw(entries: List[DrawService.DrawEntry]): F[DrawService.DrawResult]
}

object DrawService {
  sealed trait DrawEntry extends Product with Serializable {
    def toPeople: Set[Person]
  }

  final case class Person(
      name: String,
      toName: String,
      phone: String
  ) extends DrawEntry {
    def toPeople: Set[Person] = Set(this)
  }
  final case class Couple(male: Person, female: Person) extends DrawEntry {
    def toPeople: Set[Person] = Set(male, female)
  }
  final case class DrawResult(result: List[(Person, Person)])

  def impl[F[_]: Applicative]: DrawService[F] = new DrawService[F] {
    def draw(entries: List[DrawEntry]): F[DrawResult] = Applicative[F].pure {
      val drawResult = evaluate(Random.shuffle(entries))
      DrawResult(drawResult)
    }

    private def evaluate(initial: List[DrawEntry]): List[(Person, Person)] = {

      @tailrec
      def innerEvaluate(
          entries: List[DrawEntry],
          left: Set[Person],
          acc: List[(Person, Person)] = List.empty
      ): List[(Person, Person)] = entries match {
        case entry :: tail =>
          val peopleToDrawFor = entry.toPeople
          val possibleValuesSet = left -- peopleToDrawFor
          val picks =
            Random.shuffle(possibleValuesSet.toList).take(peopleToDrawFor.size)
          val result = peopleToDrawFor.zip(picks).toList ++ acc

          innerEvaluate(tail, left -- picks, result)

        case Nil => acc
      }
      val initialPeople = initial.flatMap(_.toPeople)

      innerEvaluate(initial, initialPeople.toSet)
    }
  }
}
