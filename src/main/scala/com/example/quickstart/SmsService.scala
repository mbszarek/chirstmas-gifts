package com.example.quickstart

import cats.effect.Concurrent
import io.circe.Decoder
import io.circe.generic.semiauto._
import org.http4s.EntityDecoder
import org.http4s.Method._
import org.http4s.UrlForm
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._

trait SmsService[F[_]] {
  def sendMessage(
      to: String,
      message: String
  ): F[SmsService.SendMessageResponse]
}

object SmsService {
  private val PhoneNumber = "<PhoneNumber>"
  private val ApiKey = "ApiKey"
  private val ApiPassword = "ApiPassword"

  final case class SendMessageResponse(messageId: String) extends AnyVal
  object SendMessageResponse {
    implicit val sendMessageResponseDecoder: Decoder[SendMessageResponse] =
      deriveDecoder[SendMessageResponse]
    implicit def sendMessageResponseEntityDecoder[F[_]: Concurrent]
        : EntityDecoder[F, SendMessageResponse] = jsonOf
  }

  def impl[F[_]: Concurrent](C: Client[F]): SmsService[F] = new SmsService[F] {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._

    def sendMessage(to: String, message: String): F[SendMessageResponse] = {
      val request = POST(
        UrlForm(
          "key" -> ApiKey,
          "password" -> ApiPassword,
          "from" -> PhoneNumber,
          "to" -> to,
          "msg" -> message
        ),
        uri"https://api2.smsplanet.pl/sms"
      )

      C.expect[SendMessageResponse](request)
    }
  }
}
