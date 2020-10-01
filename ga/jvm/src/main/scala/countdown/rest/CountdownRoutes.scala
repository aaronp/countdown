package countdown.rest

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import countdown.rest.Service.{CountdownRequest, CountdownResponse}
import io.circe.Json
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.{
  OptionalQueryParamDecoderMatcher,
  QueryParamDecoderMatcher
}
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Response}

object CountdownRoutes {

  implicit def responseEntityEncoder[F[_] : Applicative]
  : EntityEncoder[F, CountdownResponse] = jsonEncoderOf

  object SeedParam extends OptionalQueryParamDecoderMatcher[Long]("seed")

  object MutationProbabilityParam extends OptionalQueryParamDecoderMatcher[Double]("mutationProbability")

  object DebugParam extends OptionalQueryParamDecoderMatcher[Boolean]("debug")

  object NumbersParam extends QueryParamDecoderMatcher[String]("numbers")

  def handle[F[_] : Sync](svc: Service[F],
                          request: CountdownRequest): F[Response[F]] = {
    val dsl = Http4sDsl[F]
    import dsl._
    for {
      response <- svc.countdown(request)
      resp <- Ok(response)
    } yield resp
  }

  def apply[F[_] : Sync](svc: Service[F]): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / IntVar(target)
        :? NumbersParam(numbers)
        +& DebugParam(debug)
        +& SeedParam(seedOpt)
        +& MutationProbabilityParam(mutationProbabilityOpt) =>
        val request: CountdownRequest = Map(
          "target" -> target.asJson,
          "seed" -> seedOpt.map(_.toString).getOrElse("").asJson,
          "mutationProbability" -> mutationProbabilityOpt.getOrElse(0.01).asJson,
          "from" -> numbers.split(",", -1).map(_.toInt).asJson,
          "debug" -> debug.getOrElse(false).asJson,
        ).asJson
        handle(svc, request)
      case req@POST -> Root =>
        val body: F[CountdownRequest] = req.as[CountdownRequest]
        body.flatMap(handle(svc, _))
      case GET -> Root =>
        Ok(
          s"""Usage:
             |GET /<target number>?numbers=<comma-separated numbers>&seed=1234
             |POST /
             |${CountdownRequest.example.asJson.spaces4}
             |""".stripMargin)
    }
  }
}
