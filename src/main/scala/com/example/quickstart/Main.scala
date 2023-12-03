package com.example.quickstart

import cats.implicits._
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import org.http4s.ember.client.EmberClientBuilder

object Main extends IOApp {
  val drawEntries: List[DrawService.DrawEntry] = List(
    // Specify input for draw - either Couple or Person
  )
  def run(args: List[String]) =
    EmberClientBuilder.default[IO].build.use { client =>
      val draw = DrawService.impl[IO]
      val smsService = SmsService.impl[IO](client)
      lazy val drawResultF: IO[DrawService.DrawResult] = IO
        .defer {
          draw
            .draw(drawEntries)
        }
        .handleErrorWith(_ => drawResultF)

      for {
        _ <- IO {
          val people = drawEntries.flatMap(_.toPeople)
          require(
            people.map(_.name).distinct.size == 9 && people
              .map(_.toName)
              .distinct
              .size == 9 && people.map(_.phone).distinct.size == 9
          )
        }
        drawResult <- drawResultF
        _ <- drawResult.result.traverse { case (from, to) =>
          val smsMessage =
            s"Hej ${from.name}! W tym roku kupujesz prezent ${to.toName} :)"

          smsService.sendMessage(from.phone, smsMessage)
        }
      } yield ExitCode.Success
    }

}
