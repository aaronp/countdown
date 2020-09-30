package countdown.rest

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import countdown.rest.Service.{CountdownRequest, CountdownResponse}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.{
  OptionalQueryParamDecoderMatcher,
  QueryParamDecoderMatcher
}
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Response}
import io.circe.syntax._

object CountdownRoutes {

  implicit def requestEntityDecoder[F[_]: Sync]
    : EntityDecoder[F, CountdownRequest] = jsonOf

  implicit def responseEntityEncoder[F[_]: Applicative]
    : EntityEncoder[F, CountdownResponse] = jsonEncoderOf

  object SeedParam extends OptionalQueryParamDecoderMatcher[Long]("seed")

  object DebugParam extends OptionalQueryParamDecoderMatcher[Boolean]("debug")

  object NumbersParam extends QueryParamDecoderMatcher[String]("numbers")

  def handle[F[_]: Sync](svc: Service[F],
                         request: CountdownRequest): F[Response[F]] = {
    val dsl = Http4sDsl[F]
    import dsl._
    for {
      response <- svc.countdown(request)
      resp <- Ok(response)
    } yield resp
  }

  def apply[F[_]: Sync](svc: Service[F]): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / IntVar(target) :? NumbersParam(numbers) +& DebugParam(
            debug) +& SeedParam(seedOpt) =>
        val request =
          CountdownRequest(target,
                           numbers.split(",", -1).map(_.trim.toInt).toSet,
                           seedOpt,
                           includeWorkingsOut = debug.getOrElse(false))
        handle(svc, request)
      case req @ POST -> Root =>
        req.as[CountdownRequest].flatMap(handle(svc, _))
      case GET -> Root =>
        Ok(s"""Usage:
             |GET /<target number>?numbers=<comma-separated numbers>&seed=1234
             |POST /
             |${CountdownRequest.example.asJson.spaces4}
             |""".stripMargin)
    }
  }
}
